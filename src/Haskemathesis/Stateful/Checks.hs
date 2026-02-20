{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- | Stateful checks for API testing.

This module provides checks that verify stateful API behavior, including:

* __Use-after-free detection__: Verifies deleted resources return 404
* __Resource availability__: Verifies created resources are accessible
* __Modification persistence__: Verifies PUT/PATCH changes persist

Unlike regular checks that examine a single request/response pair,
stateful checks examine the accumulated test state and may trigger
additional API calls to verify assertions.

=== Example

@
import Haskemathesis.Stateful.Checks
import Haskemathesis.Stateful.Types

-- After running a CRUD sequence
let checks = [useAfterFree, ensureResourceAvailability]
results <- runStatefulChecks executor ops state checks
case results of
    [] -> putStrLn "All stateful checks passed"
    failures -> mapM_ printFailure failures
@

=== Check Types

Stateful checks return 'StatefulCheckResult' which captures:

* Which check was run
* The resource being verified
* The verification request/response
* Success or failure with details
-}
module Haskemathesis.Stateful.Checks (
    -- * Stateful Check Type
    StatefulCheck (..),
    StatefulCheckResult (..),
    StatefulFailure (..),

    -- * Built-in Checks
    useAfterFree,
    ensureResourceAvailability,
    ensureModificationPersisted,

    -- * Running Checks
    runStatefulChecks,
    runStatefulCheck,

    -- * Helpers
    buildResourceRequest,
    findDeletedResources,
    findCreatedResources,
    findModifiedResources,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Haskemathesis.Execute.Types (
    ApiRequest (..),
    ApiResponse (..),
    ExecutorWithTimeout,
 )
import Haskemathesis.OpenApi.Types (
    ResolvedOperation (..),
 )
import Haskemathesis.Stateful.Types (
    ResourceRef (..),
    TestState (..),
 )
import Network.HTTP.Types (methodGet)

{- | A stateful check that verifies API behavior across operations.

Unlike regular checks that examine a single request/response,
stateful checks examine the accumulated test state and may
execute additional requests to verify assertions.

=== Fields

* 'scName' - Human-readable name for reporting
* 'scRun' - Function that runs the check, given an executor,
  available operations, and current test state
-}
data StatefulCheck = StatefulCheck
    { scName :: Text
    -- ^ Human-readable name of the check
    , scRun ::
        ExecutorWithTimeout ->
        [ResolvedOperation] ->
        TestState ->
        IO [StatefulCheckResult]
    -- ^ Function that performs the check
    }

{- | Result of running a stateful check.

Captures whether the check passed or failed, along with
context about which resource was being verified.
-}
data StatefulCheckResult
    = StatefulCheckPassed
        { scrCheckName :: Text
        -- ^ Name of the check that passed
        , scrResource :: Maybe ResourceRef
        -- ^ Resource that was verified (if applicable)
        }
    | StatefulCheckFailed StatefulFailure
    deriving (Eq, Show)

{- | Details of a stateful check failure.

Provides comprehensive information for debugging and reporting,
including the verification request/response and expected behavior.
-}
data StatefulFailure = StatefulFailure
    { sfCheckName :: Text
    -- ^ Name of the check that failed
    , sfMessage :: Text
    -- ^ Human-readable description of failure
    , sfResource :: ResourceRef
    -- ^ Resource that failed verification
    , sfVerificationRequest :: Maybe ApiRequest
    -- ^ Request sent to verify the assertion
    , sfVerificationResponse :: Maybe ApiResponse
    -- ^ Response received during verification
    , sfExpectedStatus :: Maybe Int
    -- ^ Expected HTTP status code
    , sfActualStatus :: Maybe Int
    -- ^ Actual HTTP status code received
    }
    deriving (Eq, Show)

{- | Check that deleted resources return 404.

This check detects "use-after-free" bugs where:

1. A resource is created (POST /resources)
2. The resource is deleted (DELETE /resources/{id})
3. Attempting to access the resource should return 404

=== How It Works

1. Finds all DELETE operations in the test history
2. For each deleted resource, builds a GET request
3. Executes the GET request
4. Verifies the response is 404 Not Found

=== Example Failure

@
UseAfterFree Check Failed:
  Resource: /users/42
  Expected: 404 Not Found
  Actual: 200 OK (resource still accessible after DELETE)
@
-}
useAfterFree :: StatefulCheck
useAfterFree =
    StatefulCheck
        { scName = "use-after-free"
        , scRun = runUseAfterFree
        }

runUseAfterFree ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    IO [StatefulCheckResult]
runUseAfterFree executor ops state =
    mapM (checkDeletedResource executor ops) (findDeletedResources state)

checkDeletedResource ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    ResourceRef ->
    IO StatefulCheckResult
checkDeletedResource executor ops resource = do
    case findGetOperation ops resource of
        Nothing ->
            -- No GET operation available, skip check
            pure $
                StatefulCheckPassed
                    { scrCheckName = "use-after-free"
                    , scrResource = Just resource
                    }
        Just getOp -> do
            let req = buildResourceRequest getOp resource
            response <- executor Nothing req
            let status = resStatusCode response
            if status == 404
                then
                    pure $
                        StatefulCheckPassed
                            { scrCheckName = "use-after-free"
                            , scrResource = Just resource
                            }
                else
                    pure $
                        StatefulCheckFailed $
                            StatefulFailure
                                { sfCheckName = "use-after-free"
                                , sfMessage =
                                    "Deleted resource is still accessible. "
                                        <> "Expected 404, got "
                                        <> T.pack (show status)
                                , sfResource = resource
                                , sfVerificationRequest = Just req
                                , sfVerificationResponse = Just response
                                , sfExpectedStatus = Just 404
                                , sfActualStatus = Just status
                                }

{- | Check that created resources are immediately accessible.

This check verifies that resources created via POST are available
for retrieval via GET.

=== How It Works

1. Finds all POST operations that created resources
2. For each created resource, builds a GET request
3. Executes the GET request
4. Verifies the response is 200 OK

=== Note

This check excludes resources that were subsequently deleted.
-}
ensureResourceAvailability :: StatefulCheck
ensureResourceAvailability =
    StatefulCheck
        { scName = "ensure-resource-availability"
        , scRun = runEnsureResourceAvailability
        }

runEnsureResourceAvailability ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    IO [StatefulCheckResult]
runEnsureResourceAvailability executor ops state =
    mapM (checkResourceAvailable executor ops) activeResources
  where
    createdResources = findCreatedResources state
    deletedResources = findDeletedResources state
    -- Exclude resources that were deleted
    activeResources = filter (notDeleted deletedResources) createdResources
    notDeleted deleted resource =
        not $ any (sameResource resource) deleted
    sameResource r1 r2 =
        rrResourcePath r1 == rrResourcePath r2
            && rrIdentifiers r1 == rrIdentifiers r2

checkResourceAvailable ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    ResourceRef ->
    IO StatefulCheckResult
checkResourceAvailable executor ops resource = do
    case findGetOperation ops resource of
        Nothing ->
            -- No GET operation available, skip check
            pure $
                StatefulCheckPassed
                    { scrCheckName = "ensure-resource-availability"
                    , scrResource = Just resource
                    }
        Just getOp -> do
            let req = buildResourceRequest getOp resource
            response <- executor Nothing req
            let status = resStatusCode response
            if status >= 200 && status < 300
                then
                    pure $
                        StatefulCheckPassed
                            { scrCheckName = "ensure-resource-availability"
                            , scrResource = Just resource
                            }
                else
                    pure $
                        StatefulCheckFailed $
                            StatefulFailure
                                { sfCheckName = "ensure-resource-availability"
                                , sfMessage =
                                    "Created resource is not accessible. "
                                        <> "Expected 2xx, got "
                                        <> T.pack (show status)
                                , sfResource = resource
                                , sfVerificationRequest = Just req
                                , sfVerificationResponse = Just response
                                , sfExpectedStatus = Just 200
                                , sfActualStatus = Just status
                                }

{- | Check that modifications (PUT/PATCH) are persisted.

This check verifies that changes made via PUT or PATCH are
reflected in subsequent GET requests.

=== How It Works

1. Finds all PUT/PATCH operations in the history
2. For each modified resource, retrieves the modification data
3. Executes a GET request
4. Verifies the response body contains the modified values

=== Limitations

Currently performs a simple presence check on top-level fields.
Does not verify nested objects or arrays in detail.
-}
ensureModificationPersisted :: StatefulCheck
ensureModificationPersisted =
    StatefulCheck
        { scName = "ensure-modification-persisted"
        , scRun = runEnsureModificationPersisted
        }

runEnsureModificationPersisted ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    IO [StatefulCheckResult]
runEnsureModificationPersisted executor ops state =
    mapM (checkModificationPersisted executor ops) activeModifications
  where
    modifications = findModifiedResources state
    deletedResources = findDeletedResources state
    -- Exclude resources that were deleted after modification
    activeModifications = filter (notDeleted deletedResources . fst) modifications
    notDeleted deleted resource =
        not $ any (sameResource resource) deleted
    sameResource r1 r2 =
        rrResourcePath r1 == rrResourcePath r2
            && rrIdentifiers r1 == rrIdentifiers r2

checkModificationPersisted ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    (ResourceRef, Value) ->
    IO StatefulCheckResult
checkModificationPersisted executor ops (resource, expectedData) = do
    case findGetOperation ops resource of
        Nothing ->
            -- No GET operation available, skip check
            pure $
                StatefulCheckPassed
                    { scrCheckName = "ensure-modification-persisted"
                    , scrResource = Just resource
                    }
        Just getOp -> do
            let req = buildResourceRequest getOp resource
            response <- executor Nothing req
            let status = resStatusCode response
            if status >= 200 && status < 300
                then case Aeson.decodeStrict (resBody response) of
                    Nothing ->
                        -- Could not parse response body
                        pure $
                            StatefulCheckFailed $
                                StatefulFailure
                                    { sfCheckName = "ensure-modification-persisted"
                                    , sfMessage = "Could not parse response body as JSON"
                                    , sfResource = resource
                                    , sfVerificationRequest = Just req
                                    , sfVerificationResponse = Just response
                                    , sfExpectedStatus = Nothing
                                    , sfActualStatus = Just status
                                    }
                    Just actualData ->
                        if containsExpectedFields expectedData actualData
                            then
                                pure $
                                    StatefulCheckPassed
                                        { scrCheckName = "ensure-modification-persisted"
                                        , scrResource = Just resource
                                        }
                            else
                                pure $
                                    StatefulCheckFailed $
                                        StatefulFailure
                                            { sfCheckName = "ensure-modification-persisted"
                                            , sfMessage =
                                                "Modified values not found in response"
                                            , sfResource = resource
                                            , sfVerificationRequest = Just req
                                            , sfVerificationResponse = Just response
                                            , sfExpectedStatus = Nothing
                                            , sfActualStatus = Just status
                                            }
                else
                    pure $
                        StatefulCheckFailed $
                            StatefulFailure
                                { sfCheckName = "ensure-modification-persisted"
                                , sfMessage =
                                    "Could not retrieve modified resource. "
                                        <> "Expected 2xx, got "
                                        <> T.pack (show status)
                                , sfResource = resource
                                , sfVerificationRequest = Just req
                                , sfVerificationResponse = Just response
                                , sfExpectedStatus = Just 200
                                , sfActualStatus = Just status
                                }

-- | Check if actual data contains all expected top-level fields.
containsExpectedFields :: Value -> Value -> Bool
containsExpectedFields (Object expected) (Object actual) =
    all fieldMatches (KM.toList expected)
  where
    fieldMatches (key, expectedVal) =
        case KM.lookup key actual of
            Nothing -> False
            Just actualVal -> valueMatches expectedVal actualVal
containsExpectedFields expected actual = expected == actual

-- | Check if values match (for simple types, exact match; for objects, containment).
valueMatches :: Value -> Value -> Bool
valueMatches (Object expected) (Object actual) =
    containsExpectedFields (Object expected) (Object actual)
valueMatches expected actual = expected == actual

{- | Run all stateful checks against the test state.

Executes each check in sequence and collects all results.
Failed checks do not prevent subsequent checks from running.

=== Example

@
let checks = [useAfterFree, ensureResourceAvailability]
results <- runStatefulChecks executor ops state checks
let failures = [f | StatefulCheckFailed f <- results]
when (not $ null failures) $
    fail $ "Stateful checks failed: " ++ show (length failures)
@
-}
runStatefulChecks ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    [StatefulCheck] ->
    IO [StatefulCheckResult]
runStatefulChecks executor ops state checks = do
    resultLists <- mapM (\check -> scRun check executor ops state) checks
    pure $ concat resultLists

{- | Run a single stateful check.

Convenience function for running a single check.
-}
runStatefulCheck ::
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestState ->
    StatefulCheck ->
    IO [StatefulCheckResult]
runStatefulCheck executor ops state check =
    scRun check executor ops state

{- | Build a GET request for a resource.

Constructs an ApiRequest to retrieve the specified resource
by interpolating path parameters from the resource identifiers.

=== Example

@
let resource = ResourceRef "createUser" "/users/{id}" (Map.singleton "id" (Number 42))
    getOp = ... -- ResolvedOperation for GET /users/{id}
    req = buildResourceRequest getOp resource
-- req.reqPath = "/users/42"
@
-}
buildResourceRequest :: ResolvedOperation -> ResourceRef -> ApiRequest
buildResourceRequest op resource =
    ApiRequest
        { reqMethod = methodGet
        , reqPath = interpolatePath (roPath op) (rrIdentifiers resource)
        , reqQueryParams = []
        , reqHeaders = []
        , reqBody = Nothing
        }

-- | Interpolate path parameters into a path template.
interpolatePath :: Text -> Map Text Value -> Text
interpolatePath = Map.foldrWithKey replaceParam
  where
    replaceParam :: Text -> Value -> Text -> Text
    replaceParam paramName paramValue =
        T.replace ("{" <> paramName <> "}") (renderValue paramValue)

-- | Render a JSON value as text for path interpolation.
renderValue :: Value -> Text
renderValue (String t) = t
renderValue (Number n) =
    -- Render integers without decimal point
    case Scientific.floatingOrInteger n of
        Left (_ :: Double) -> T.pack $ show n
        Right i -> T.pack $ show (i :: Integer)
renderValue (Bool True) = "true"
renderValue (Bool False) = "false"
renderValue Null = "null"
renderValue (Array _) = ""
renderValue (Object _) = ""

{- | Find all resources that were deleted during the test.

Scans the test history for DELETE operations and extracts
resource references from them.
-}
findDeletedResources :: TestState -> [ResourceRef]
findDeletedResources state =
    mapMaybe extractDeletedResource (tsHistory state)
  where
    extractDeletedResource (opLabel, req, res)
        | "DELETE" `T.isPrefixOf` opLabel || isDeletePath (reqPath req) =
            if resStatusCode res >= 200 && resStatusCode res < 300
                then
                    Just $
                        ResourceRef
                            { rrOperationId = opLabel
                            , rrResourcePath = reqPath req
                            , rrIdentifiers = extractPathIds (reqPath req)
                            }
                else Nothing
        | otherwise = Nothing

    isDeletePath path =
        -- Heuristic: DELETE operations typically have an ID in the path
        "{" `T.isInfixOf` path || hasIdSegment path

    hasIdSegment path =
        any isIdSegment (T.splitOn "/" path)

    isIdSegment segment =
        not (T.null segment)
            && (T.all isDigit segment || isUuidLike segment)

    isUuidLike segment =
        -- Use compareLength for efficient short-circuiting before counting dashes
        T.compareLength segment 36 == EQ && T.count "-" segment == 4

{- | Find all resources that were created during the test.

Returns resources from tsCreatedResources that have not been deleted.
-}
findCreatedResources :: TestState -> [ResourceRef]
findCreatedResources = tsCreatedResources

{- | Find all resources that were modified (PUT/PATCH) during the test.

Returns pairs of (ResourceRef, ModificationData) for each modification.
-}
findModifiedResources :: TestState -> [(ResourceRef, Value)]
findModifiedResources state =
    mapMaybe extractModification (tsHistory state)
  where
    extractModification (opLabel, req, res)
        | isPutOrPatch opLabel =
            if resStatusCode res >= 200 && resStatusCode res < 300
                then case reqBody req of
                    Just (_, bodyBytes) ->
                        case Aeson.decodeStrict bodyBytes of
                            Just bodyValue ->
                                Just
                                    ( ResourceRef
                                        { rrOperationId = opLabel
                                        , rrResourcePath = reqPath req
                                        , rrIdentifiers = extractPathIds (reqPath req)
                                        }
                                    , bodyValue
                                    )
                            Nothing -> Nothing
                    Nothing -> Nothing
                else Nothing
        | otherwise = Nothing

    isPutOrPatch label =
        "PUT" `T.isPrefixOf` label
            || "PATCH" `T.isPrefixOf` label
            || "update" `T.isInfixOf` T.toLower label
            || "modify" `T.isInfixOf` T.toLower label

-- | Extract path segment IDs from a concrete path.
extractPathIds :: Text -> Map Text Value
extractPathIds path =
    Map.fromList $ mapMaybe extractId (zip ([0 ..] :: [Int]) segments)
  where
    segments = filter (not . T.null) $ T.splitOn "/" path
    extractId :: (Int, Text) -> Maybe (Text, Value)
    extractId (idx, segment)
        | isNumeric segment = do
            -- Use readMaybe to safely parse the number, avoiding partial 'read'
            n <- readMaybe (T.unpack segment) :: Maybe Integer
            let key = "id" <> if idx == 0 then "" else T.pack (show idx)
            Just (key, Number (fromInteger n))
        | isUuidLike segment =
            Just ("id" <> if idx == 0 then "" else T.pack (show idx), String segment)
        | otherwise = Nothing

    isNumeric s = not (T.null s) && T.all isDigit s
    -- Use compareLength for efficient short-circuiting before counting dashes
    isUuidLike s = T.compareLength s 36 == EQ && T.count "-" s == 4

-- | Find a GET operation that can retrieve the given resource.
findGetOperation :: [ResolvedOperation] -> ResourceRef -> Maybe ResolvedOperation
findGetOperation ops resource =
    case filter isMatchingGet ops of
        (op : _) -> Just op
        [] -> Nothing
  where
    isMatchingGet op =
        roMethod op == "GET"
            && pathMatches (roPath op) (rrResourcePath resource)

    pathMatches template concrete =
        -- Check if template matches concrete path structure
        let templateSegments = T.splitOn "/" template
            concreteSegments = T.splitOn "/" concrete
         in -- Use lazy length comparison to avoid traversing lists twice
            -- and to work safely with any list size
            sameLength templateSegments concreteSegments
                && all segmentMatches (zip templateSegments concreteSegments)

    -- Safe length comparison that short-circuits and works with any list
    sameLength :: [a] -> [b] -> Bool
    sameLength [] [] = True
    sameLength (_ : xs) (_ : ys) = sameLength xs ys
    sameLength _ _ = False

    segmentMatches (t, c)
        | "{" `T.isPrefixOf` t && "}" `T.isSuffixOf` t = True
        | otherwise = t == c
