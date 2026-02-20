{-# LANGUAGE OverloadedStrings #-}

{- | High-level property generation for resolved operations and specs.

This module provides the main entry points for generating Hedgehog properties
from OpenAPI specifications. Properties can be generated for individual
operations or entire specs, with various configuration options.

=== Basic Usage

Generate properties for all operations in a spec:

@
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.Property (propertiesForSpec)
import Haskemathesis.Check.Standard (defaultChecks)

main :: IO ()
main = do
    Right spec <- loadOpenApiFile "api.yaml"
    let ops = resolveOperations spec
        props = propertiesForSpec Nothing defaultChecks myExecutor ops
    -- Run properties with your favorite test framework
@

=== Configuration

For more control, use 'propertiesForSpecWithConfig':

@
import Haskemathesis.Config

myConfig :: TestConfig
myConfig = defaultTestConfig
    { tcPropertyCount = 200
    , tcNegativeTesting = True
    , tcOperationFilter = filterByTag "public"
    }
@

=== Stateful Testing

For stateful API testing that chains operations together:

@
import Haskemathesis.Property (propertyStateful)

-- Run stateful CRUD sequences
prop = propertyStateful spec config httpExecutor ops
@
-}
module Haskemathesis.Property (
    -- * Single Operation Properties
    propertyForOperation,
    propertyForOperationWithConfig,
    propertyForOperationNegative,

    -- * Spec-Level Properties
    propertiesForSpec,
    propertiesForSpecWithConfig,
    propertiesForSpecNegative,

    -- * Stateful Testing
    propertyStateful,
    propertiesForSpecStateful,
    executeStatefulSequence,
    executeStatefulStep,

    -- * Stateful Testing Helpers (for testing)
    runStatefulChecksProperty,
    isStatefulFailure,
    renderStatefulFailure,

    -- * Timeout Utilities
    effectiveTimeout,
)
where

import Control.Monad (foldM)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (withFrozenCallStack)
import Haskemathesis.Auth.Config (applyAuthForOperation)
import Haskemathesis.Check.Negative (negativeTestRejection)
import Haskemathesis.Check.Standard.Helpers (operationLabel)
import Haskemathesis.Check.Types (Check (..), CheckResult (..), FailureDetail)
import Haskemathesis.Config (TestConfig (..))
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..), BaseUrl, ExecutorWithTimeout)
import Haskemathesis.Gen.Negative (genNegativeRequest, renderNegativeMutation)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Report.Render (renderFailureDetailAnsi)
import Haskemathesis.Stateful.Checks (
    StatefulCheckResult (..),
    StatefulFailure (..),
    runStatefulChecks,
 )
import Haskemathesis.Stateful.Generator (genStatefulRequest, updateState)
import Haskemathesis.Stateful.Heuristics (inferLinks)
import Haskemathesis.Stateful.Links (extractLinks)
import Haskemathesis.Stateful.Sequence (findOperationByLabel, genOperationSequence)
import Haskemathesis.Stateful.Types (
    OperationSequence (..),
    SequenceStep (..),
    TestState (..),
    emptyState,
 )
import Hedgehog (Gen, Property, evalIO, failure, forAll, property, success, withTests)
import Hedgehog.Internal.Property (PropertyT, footnote)
import System.Environment (lookupEnv)

{- | Generate a Hedgehog property for a single operation using basic configuration.

This is the simplest way to create a property for an operation. The property
will generate random requests based on the OpenAPI schema and execute them
against your API, then run the provided checks on the response.

=== Parameters

* @mBase@ - Optional base URL for rendering failure reports (can be 'Nothing')
* @checks@ - List of 'Check' functions to validate responses
* @execute@ - Function to execute an 'ApiRequest' and return an 'ApiResponse'
* @op@ - The 'ResolvedOperation' to generate tests for

=== Example

@
let prop = propertyForOperation (Just "http://localhost:8080") checks httpExecutor myOperation
@
-}
propertyForOperation ::
    Maybe BaseUrl ->
    [Check] ->
    ExecutorWithTimeout ->
    ResolvedOperation ->
    Property
propertyForOperation mBase checks execute op =
    property $ do
        req <- forAllNoLoc (genApiRequest op)
        -- No config available, so no timeout for basic version
        res <- evalIO (execute Nothing req)
        runChecks mBase checks req res op

{- | Generate a property with full configuration support including authentication.

This function provides more control than 'propertyForOperation' by accepting
a 'TestConfig' record. It supports:

* Authentication via 'tcAuthConfig'
* Custom property counts via 'tcPropertyCount'
* Operation filtering via 'tcOperationFilter'
* Automatic timeout handling for streaming endpoints

=== Parameters

* @openApi@ - The full OpenAPI specification (needed for auth resolution)
* @config@ - The 'TestConfig' controlling test generation
* @execute@ - Timeout-aware executor (see 'ExecutorWithTimeout')
* @op@ - The operation to test

=== Example

@
config = defaultTestConfig { tcPropertyCount = 200 }
prop = propertyForOperationWithConfig spec config httpExecutor myOperation
@
-}
propertyForOperationWithConfig ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    ResolvedOperation ->
    Property
propertyForOperationWithConfig openApi config execute op =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            req <- forAllNoLoc (genApiRequest op)
            let req' = applyConfigToRequest openApi config op req
            let timeout = effectiveTimeout config op
            res <- evalIO (execute timeout req')
            runChecks (tcBaseUrl config) (tcChecks config) req' res op

{- | Generate a negative testing property for an operation.

Negative testing generates /invalid/ requests to verify that the API
properly rejects malformed input. This property generates mutations
that violate the OpenAPI schema (e.g., missing required fields,
wrong content types, invalid values) and verifies the API rejects them.

=== When to Use

Use this when you want to verify your API's error handling and
input validation. The API should return 4xx status codes for
these invalid requests.

=== Parameters

* @openApi@ - The full OpenAPI specification
* @config@ - The 'TestConfig' (negative testing is enabled separately)
* @execute@ - Timeout-aware executor
* @op@ - The operation to test

=== Example

@
config = defaultTestConfig { tcPropertyCount = 50 }
negProp = propertyForOperationNegative spec config httpExecutor myOperation
@
-}
propertyForOperationNegative ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    ResolvedOperation ->
    Property
propertyForOperationNegative openApi config execute op =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            mReq <- forAllNoLoc (genNegativeRequest op)
            case mReq of
                Nothing -> success
                Just (req, mutation) -> do
                    let req' = applyConfigToRequest openApi config op req
                    let timeout = effectiveTimeout config op
                    res <- evalIO (execute timeout req')
                    case negativeTestRejection (renderNegativeMutation mutation) req' res op of
                        CheckPassed -> success
                        CheckFailed detail -> reportFailure (tcBaseUrl config) detail

{- | Generate properties for all operations in a spec using basic configuration.

This is the simplest way to generate a complete test suite from an
OpenAPI specification. It creates a list of named properties, one
for each resolved operation.

=== Return Value

Returns a list of tuples where:
* First element: A descriptive label (operation ID or method+path)
* Second element: The Hedgehog 'Property' for testing that operation

=== Parameters

* @mBase@ - Optional base URL for rendering failure reports
* @checks@ - List of 'Check' functions to validate responses
* @execute@ - Function to execute requests
* @ops@ - List of resolved operations (from 'resolveOperations')

=== Example

@
main :: IO ()
main = do
    Right spec <- loadOpenApiFile "api.yaml"
    let ops = resolveOperations spec
        props = propertiesForSpec Nothing defaultChecks httpExecutor ops
    -- Run with your test framework
@
-}
propertiesForSpec ::
    Maybe BaseUrl ->
    [Check] ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpec mBase checks execute ops =
    [ (operationLabel op, propertyForOperation mBase checks execute op)
    | op <- ops
    ]

{- | Generate properties with full configuration support.

This function provides the most flexibility for generating properties.
It supports authentication, operation filtering, custom check sets,
automatic timeout handling for streaming endpoints, and can generate
both positive and negative test cases.

=== Negative Testing

When 'tcNegativeTesting' is 'True', this function generates both
normal properties and negative testing properties (prefixed with
"NEGATIVE:"). Negative tests verify that the API properly rejects
invalid requests.

=== Operation Filtering

Only operations that satisfy 'tcOperationFilter' will have properties
generated. This is useful for testing subsets of your API.

=== Timeout Handling

Streaming endpoints (SSE, NDJSON) are automatically detected and
tested with timeouts to prevent tests from hanging. The timeout
is determined by (in order of precedence):

1. Operation's @x-timeout@ extension (if set in OpenAPI spec)
2. 'tcStreamingTimeout' from config (if operation is streaming)
3. No timeout (for non-streaming operations without @x-timeout@)

=== Parameters

* @openApi@ - The full OpenAPI specification (needed for auth resolution)
* @config@ - The 'TestConfig' controlling all aspects of generation
* @execute@ - Timeout-aware executor (see 'ExecutorWithTimeout')
* @ops@ - List of resolved operations

=== Example

@
config = defaultTestConfig
    { tcPropertyCount = 200
    , tcNegativeTesting = True
    , tcOperationFilter = filterByTag "public"
    }
props = propertiesForSpecWithConfig spec config httpExecutor ops
@
-}
propertiesForSpecWithConfig ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpecWithConfig openApi config execute ops =
    normalProps
        <> if tcNegativeTesting config
            then negativeProps
            else []
  where
    normalProps =
        [ (operationLabel op, propertyForOperationWithConfig openApi config execute op)
        | op <- ops
        , tcOperationFilter config op
        ]
    negativeProps =
        [ ("NEGATIVE: " <> operationLabel op, propertyForOperationNegative openApi config execute op)
        | op <- ops
        , tcOperationFilter config op
        ]

{- | Generate only negative testing properties.

This is a convenience function that generates only negative test properties
for all operations. Negative tests verify that the API properly rejects
invalid requests (mutations that violate the OpenAPI schema).

=== Use Cases

* Testing input validation and error handling
* Verifying security controls reject malformed requests
* Dedicated negative testing suites

=== Parameters

* @openApi@ - The full OpenAPI specification
* @config@ - The 'TestConfig' (negative testing flag is ignored, always enabled)
* @execute@ - Timeout-aware executor
* @ops@ - List of resolved operations

=== Example

@
-- Dedicated negative testing suite
negativeProps = propertiesForSpecNegative spec defaultConfig httpExecutor ops
@
-}
propertiesForSpecNegative ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpecNegative openApi config =
    propertiesForSpecWithConfig openApi (config{tcNegativeTesting = True})

-- | Internal helper to apply auth and config headers to a request.
applyConfigToRequest :: OpenApi -> TestConfig -> ResolvedOperation -> ApiRequest -> ApiRequest
applyConfigToRequest openApi config op req =
    let reqAuth = case tcAuthConfig config of
            Nothing -> req
            Just auth -> applyAuthForOperation openApi auth op req
     in reqAuth{reqHeaders = tcHeaders config ++ reqHeaders reqAuth}

{- | Internal helper to report a failure with seed and details.
Uses footnote instead of annotate to avoid showing source location in output.
Uses withFrozenCallStack to hide internal library locations from failure call.
-}
reportFailure :: Maybe BaseUrl -> FailureDetail -> PropertyT IO ()
reportFailure mBase detail = do
    mSeed <- evalIO (lookupEnv "HEDGEHOG_SEED")
    let seedText = T.pack <$> mSeed
    footnote (T.unpack (renderFailureDetailAnsi mBase seedText detail))
    withFrozenCallStack failure

-- | Internal helper to run all checks and report first failure.
runChecks ::
    Maybe BaseUrl ->
    [Check] ->
    ApiRequest ->
    ApiResponse ->
    ResolvedOperation ->
    PropertyT IO ()
runChecks mBase checks req res op =
    case firstFailure of
        Nothing -> success
        Just detail -> reportFailure mBase detail
  where
    firstFailure =
        case [detail | Check{checkRun = run} <- checks, CheckFailed detail <- [run req res op]] of
            [] -> Nothing
            (detail : _) -> Just detail

{- | Compute the effective timeout for an operation.

This function determines the appropriate timeout to use for an operation
based on the following precedence:

1. __Operation's @x-timeout@__: If the OpenAPI spec has @x-timeout@ set
   on the operation, that value is used (in milliseconds).

2. __Config's streaming timeout__: If the operation is detected as
   streaming (has @text/event-stream@ or @application/x-ndjson@ content
   types), the 'tcStreamingTimeout' from config is used.

3. __No timeout__: For non-streaming operations without @x-timeout@,
   'Nothing' is returned (use HTTP client default).

=== Example

@
let timeout = effectiveTimeout config operation
-- timeout is Just 2000 if x-timeout: 2000 in spec
-- timeout is Just 1000 if streaming with default config
-- timeout is Nothing for normal operations
@
-}
effectiveTimeout :: TestConfig -> ResolvedOperation -> Maybe Int
effectiveTimeout config op =
    case roTimeout op of
        -- Explicit x-timeout takes precedence
        Just t -> Just t
        Nothing
            -- Streaming operations use config timeout
            | roIsStreaming op -> tcStreamingTimeout config
            -- Non-streaming: no timeout
            | otherwise -> Nothing

{- | Internal helper: forAll with frozen call stack.
This hides internal library source locations from Hedgehog output
while still showing the generated value.
-}
forAllNoLoc :: (Monad m, Show a) => Gen a -> PropertyT m a
forAllNoLoc gen = withFrozenCallStack (forAll gen)

-- ============================================================================
-- Stateful Testing
-- ============================================================================

{- | Run a stateful test that chains multiple API operations together.

Stateful testing generates sequences of API operations where responses
from earlier operations inform requests of later operations. This enables
testing realistic scenarios like:

@
POST /users         -- Create user, returns {id: 123}
GET /users/123      -- Fetch using id from POST response
PUT /users/123      -- Update using same id
DELETE /users/123   -- Remove the resource
GET /users/123      -- Should return 404 (use-after-free check)
@

=== How It Works

1. Combines explicit OpenAPI links with heuristically-inferred links
2. Generates a random valid operation sequence following these links
3. Executes each operation, extracting values for subsequent requests
4. Runs regular checks after each operation
5. Runs stateful checks after the sequence completes (e.g., use-after-free)

=== Parameters

* @openApi@ - The full OpenAPI specification
* @config@ - Test configuration (should have stateful options set)
* @execute@ - Timeout-aware executor
* @ops@ - List of resolved operations

=== Example

@
config = defaultTestConfig
    { tcStatefulTesting = True
    , tcMaxSequenceLength = 5
    }
prop = propertyStateful spec config httpExecutor ops
@
-}
propertyStateful ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    Property
propertyStateful openApi config execute ops =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            -- Combine explicit and inferred links
            let explicitLinks = extractLinks openApi
                inferredLinks = inferLinks ops
                allLinks = explicitLinks <> inferredLinks
                maxLen = tcMaxSequenceLength config

            -- Generate a random valid sequence
            sequence' <- forAllNoLoc (genOperationSequence maxLen ops allLinks)

            -- Execute the sequence and accumulate state
            finalState <- executeStatefulSequence openApi config execute ops sequence'

            -- Run stateful checks on the final state
            runStatefulChecksProperty config execute ops finalState

{- | Generate stateful properties for all operations that can start sequences.

Creates properties that test CRUD-like sequences starting from each
"creator" operation (typically POSTs). Unlike 'propertiesForSpecWithConfig',
these properties test multiple operations together in sequences.

=== Parameters

* @openApi@ - The full OpenAPI specification
* @config@ - Test configuration with stateful options
* @execute@ - Timeout-aware executor
* @ops@ - List of resolved operations

=== Return Value

Returns a list of tuples where:
* First element: "STATEFUL: " prefix + operation label
* Second element: The stateful 'Property'

=== Example

@
config = defaultTestConfig { tcMaxSequenceLength = 5 }
props = propertiesForSpecStateful spec config httpExecutor ops
-- Returns: [("STATEFUL: createUser", ...), ("STATEFUL: createPost", ...)]
@
-}
propertiesForSpecStateful ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpecStateful openApi config execute ops =
    [("STATEFUL: API Sequences", propertyStateful openApi config execute filteredOps)]
  where
    filteredOps = filter (tcOperationFilter config) ops

{- | Execute a complete stateful sequence and return the final state.

This function executes each step in the sequence, updating state after
each successful request. It also executes cleanup steps at the end.

=== Parameters

* @openApi@ - OpenAPI spec for auth resolution
* @config@ - Test configuration
* @execute@ - Timeout-aware executor
* @ops@ - Available operations (for looking up by ID)
* @sequence'@ - The sequence to execute

=== Return Value

Returns the final 'TestState' after executing all steps.

=== Side Effects

* Executes HTTP requests via the executor
* May abort with a Hedgehog failure if checks fail
-}
executeStatefulSequence ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    OperationSequence ->
    PropertyT IO TestState
executeStatefulSequence openApi config execute ops sequence' = do
    -- Execute main steps
    stateAfterMain <- foldM (executeStatefulStep openApi config execute ops) emptyState (osSteps sequence')

    -- Execute cleanup steps (even if main steps failed, but we continue after failure)
    -- For now, cleanup runs after main steps complete successfully
    foldM (executeStatefulStep openApi config execute ops) stateAfterMain (osCleanup sequence')

{- | Execute a single step in a stateful sequence.

This function:

1. Looks up the operation by ID
2. Generates a request using state for bound parameters
3. Applies auth and config headers
4. Executes the request
5. Runs standard checks
6. Updates state with extracted values from response

=== Parameters

* @openApi@ - OpenAPI spec for auth resolution
* @config@ - Test configuration
* @execute@ - Timeout-aware executor
* @ops@ - Available operations
* @state@ - Current test state
* @step@ - The step to execute

=== Return Value

Returns the updated 'TestState' after executing the step.

=== Failures

If the operation ID is not found, or if checks fail, the property fails.
-}
executeStatefulStep ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    SequenceStep ->
    PropertyT IO TestState
executeStatefulStep openApi config execute ops state step = do
    -- Look up the operation
    let opLabel = ssOperationId step
    case findOperationByLabel opLabel ops of
        Nothing -> do
            footnote $ "Operation not found: " <> T.unpack opLabel
            withFrozenCallStack failure
        Just op -> do
            -- Generate request using state for bound parameters
            req <- forAllNoLoc (genStatefulRequest state step op)

            -- Apply auth and config headers
            let req' = applyConfigToRequest openApi config op req

            -- Execute the request
            let timeout = effectiveTimeout config op
            res <- evalIO (execute timeout req')

            -- Run standard checks
            runChecks (tcBaseUrl config) (tcChecks config) req' res op

            -- Update state with extracted values
            pure $ updateState opLabel op req' res state

-- | Internal helper to run stateful checks and report failures.
runStatefulChecksProperty ::
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    PropertyT IO ()
runStatefulChecksProperty config execute ops state = do
    let checks = tcStatefulChecks config
    results <- evalIO $ runStatefulChecks execute ops state checks
    case filter isStatefulFailure results of
        [] -> success
        failures -> do
            -- Report all failures
            mapM_ reportStatefulFailure failures
            withFrozenCallStack failure

-- | Check if a stateful check result is a failure.
isStatefulFailure :: StatefulCheckResult -> Bool
isStatefulFailure (StatefulCheckFailed _) = True
isStatefulFailure (StatefulCheckPassed _ _) = False

-- | Report a stateful check failure.
reportStatefulFailure :: StatefulCheckResult -> PropertyT IO ()
reportStatefulFailure (StatefulCheckPassed _ _) = pure ()
reportStatefulFailure (StatefulCheckFailed sf) =
    footnote (T.unpack (renderStatefulFailure sf))

-- | Render a stateful failure as human-readable text.
renderStatefulFailure :: StatefulFailure -> Text
renderStatefulFailure sf =
    T.unlines
        [ "Stateful Check Failed: " <> sfCheckName sf
        , "Message: " <> sfMessage sf
        , "Resource: " <> maybe "N/A" (T.pack . show) (sfVerificationRequest sf >>= Just . reqPath)
        , case (sfExpectedStatus sf, sfActualStatus sf) of
            (Just expected, Just actual) ->
                "Expected status: " <> T.pack (show expected) <> ", Actual: " <> T.pack (show actual)
            (Nothing, Just actual) ->
                "Actual status: " <> T.pack (show actual)
            (Just expected, Nothing) ->
                "Expected status: " <> T.pack (show expected)
            (Nothing, Nothing) ->
                ""
        ]
