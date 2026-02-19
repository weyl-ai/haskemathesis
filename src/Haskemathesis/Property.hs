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
)
where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import qualified Data.Text as T
import Haskemathesis.Auth.Config (applyAuthForOperation)
import Haskemathesis.Check.Negative (negativeTestRejection)
import Haskemathesis.Check.Standard.Helpers (operationLabel)
import Haskemathesis.Check.Types (Check (..), CheckResult (..), FailureDetail)
import Haskemathesis.Config (TestConfig (..))
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse, BaseUrl)
import Haskemathesis.Gen.Negative (genNegativeRequest, renderNegativeMutation)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Report.Render (renderFailureDetailAnsi)
import Hedgehog (Property, annotate, evalIO, failure, forAll, property, success, withTests)
import Hedgehog.Internal.Property (PropertyT)
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
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Property
propertyForOperation mBase checks execute op =
    property $ do
        req <- forAll (genApiRequest op)
        res <- evalIO (execute req)
        runChecks mBase checks req res op

{- | Generate a property with full configuration support including authentication.

This function provides more control than 'propertyForOperation' by accepting
a 'TestConfig' record. It supports:

* Authentication via 'tcAuthConfig'
* Custom property counts via 'tcPropertyCount'
* Operation filtering via 'tcOperationFilter'

=== Parameters

* @openApi@ - The full OpenAPI specification (needed for auth resolution)
* @config@ - The 'TestConfig' controlling test generation
* @execute@ - Function to execute requests
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
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Property
propertyForOperationWithConfig openApi config execute op =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            req <- forAll (genApiRequest op)
            let req' = applyConfigToRequest openApi config op req
            res <- evalIO (execute req')
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
* @execute@ - Function to execute requests
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
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Property
propertyForOperationNegative openApi config execute op =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            mReq <- forAll (genNegativeRequest op)
            case mReq of
                Nothing -> success
                Just (req, mutation) -> do
                    let req' = applyConfigToRequest openApi config op req
                    res <- evalIO (execute req')
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
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpec mBase checks execute ops =
    [ (operationLabel op, propertyForOperation mBase checks execute op)
    | op <- ops
    ]

{- | Generate properties with full configuration support.

This function provides the most flexibility for generating properties.
It supports authentication, operation filtering, custom check sets,
and can generate both positive and negative test cases.

=== Negative Testing

When 'tcNegativeTesting' is 'True', this function generates both
normal properties and negative testing properties (prefixed with
"NEGATIVE:"). Negative tests verify that the API properly rejects
invalid requests.

=== Operation Filtering

Only operations that satisfy 'tcOperationFilter' will have properties
generated. This is useful for testing subsets of your API.

=== Parameters

* @openApi@ - The full OpenAPI specification (needed for auth resolution)
* @config@ - The 'TestConfig' controlling all aspects of generation
* @execute@ - Function to execute requests
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
    (ApiRequest -> IO ApiResponse) ->
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
* @execute@ - Function to execute requests
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
    (ApiRequest -> IO ApiResponse) ->
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

-- | Internal helper to report a failure with seed and details.
reportFailure :: Maybe BaseUrl -> FailureDetail -> PropertyT IO ()
reportFailure mBase detail = do
    mSeed <- evalIO (lookupEnv "HEDGEHOG_SEED")
    let seedText = T.pack <$> mSeed
    annotate (T.unpack (renderFailureDetailAnsi mBase seedText detail))
    failure

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
