{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for stateful checks.
module Haskemathesis.Test.Properties.Stateful.Checks (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (secondsToNominalDiffTime)
import Hedgehog (Property, assert, property, (===))
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Stateful.Checks
import Haskemathesis.Stateful.Types (
    ResourceRef (..),
    TestState (..),
    emptyState,
 )
import Haskemathesis.Test.Support (itProp)
import Network.HTTP.Types (Method, methodDelete, methodGet, methodPut)

tests :: Spec
tests = describe "Stateful.Checks" $ do
    describe "buildResourceRequest" $ do
        itProp "creates GET request" propBuildResourceRequestMethod
        itProp "interpolates path parameters" propBuildResourceRequestPath
        itProp "handles multiple parameters" propBuildResourceRequestMultipleParams

    describe "findDeletedResources" $ do
        itProp "finds DELETE operations in history" propFindDeletedResources
        itProp "ignores failed DELETE operations" propFindDeletedResourcesIgnoresFailed
        itProp "returns empty for no history" propFindDeletedResourcesEmpty

    describe "findCreatedResources" $ do
        itProp "returns tsCreatedResources" propFindCreatedResources

    describe "findModifiedResources" $ do
        itProp "finds PUT operations" propFindModifiedResourcesPut
        itProp "finds PATCH operations" propFindModifiedResourcesPatch
        itProp "ignores GET operations" propFindModifiedResourcesIgnoresGet

    describe "useAfterFree" $ do
        itProp "check name is correct" propUseAfterFreeName

    describe "ensureResourceAvailability" $ do
        itProp "check name is correct" propEnsureResourceAvailabilityName

    describe "ensureModificationPersisted" $ do
        itProp "check name is correct" propEnsureModificationPersistedName

    describe "StatefulCheckResult" $ do
        itProp "passed result has check name" propPassedHasCheckName
        itProp "failed result has details" propFailedHasDetails

-- buildResourceRequest properties

propBuildResourceRequestMethod :: Property
propBuildResourceRequestMethod = property $ do
    let op = mkGetOp "/users/{id}"
        resource = mkResource "/users/{id}" [("id", Number 42)]
        req = buildResourceRequest op resource
    reqMethod req === methodGet

propBuildResourceRequestPath :: Property
propBuildResourceRequestPath = property $ do
    let op = mkGetOp "/users/{id}"
        resource = mkResource "/users/{id}" [("id", Number 42)]
        req = buildResourceRequest op resource
    reqPath req === "/users/42"

propBuildResourceRequestMultipleParams :: Property
propBuildResourceRequestMultipleParams = property $ do
    let op = mkGetOp "/orgs/{orgId}/users/{userId}"
        resource = mkResource "/orgs/{orgId}/users/{userId}" [("orgId", Number 1), ("userId", Number 42)]
        req = buildResourceRequest op resource
    reqPath req === "/orgs/1/users/42"

-- findDeletedResources properties

propFindDeletedResources :: Property
propFindDeletedResources = property $ do
    let deleteReq = mkRequest methodDelete "/users/42"
        deleteRes = mkResponse 204
        state =
            emptyState
                { tsHistory = [("DELETE /users/42", deleteReq, deleteRes)]
                }
        deleted = findDeletedResources state
    assert $ not $ null deleted

propFindDeletedResourcesIgnoresFailed :: Property
propFindDeletedResourcesIgnoresFailed = property $ do
    let deleteReq = mkRequest methodDelete "/users/42"
        deleteRes = mkResponse 500 -- Failed
        state =
            emptyState
                { tsHistory = [("DELETE /users/42", deleteReq, deleteRes)]
                }
        deleted = findDeletedResources state
    deleted === []

propFindDeletedResourcesEmpty :: Property
propFindDeletedResourcesEmpty = property $ do
    let deleted = findDeletedResources emptyState
    deleted === []

-- findCreatedResources properties

propFindCreatedResources :: Property
propFindCreatedResources = property $ do
    let resource = mkResource "/users/{id}" [("id", Number 42)]
        state = emptyState{tsCreatedResources = [resource]}
        created = findCreatedResources state
    -- Use pattern matching to verify exactly one resource was found
    case created of
        [r] -> rrResourcePath r === "/users/{id}"
        [] -> assert False
        _multipleResources -> assert False

-- findModifiedResources properties

propFindModifiedResourcesPut :: Property
propFindModifiedResourcesPut = property $ do
    let putReq =
            ApiRequest
                { reqMethod = methodPut
                , reqPath = "/users/42"
                , reqQueryParams = []
                , reqHeaders = []
                , reqBody = Just ("application/json", "{\"name\":\"updated\"}")
                }
        putRes = mkResponse 200
        state =
            emptyState
                { tsHistory = [("PUT /users/42", putReq, putRes)]
                }
        modified = findModifiedResources state
    assert $ not $ null modified

propFindModifiedResourcesPatch :: Property
propFindModifiedResourcesPatch = property $ do
    let patchReq =
            ApiRequest
                { reqMethod = methodPut -- Using PUT method but PATCH label
                , reqPath = "/users/42"
                , reqQueryParams = []
                , reqHeaders = []
                , reqBody = Just ("application/json", "{\"name\":\"patched\"}")
                }
        patchRes = mkResponse 200
        state =
            emptyState
                { tsHistory = [("PATCH /users/42", patchReq, patchRes)]
                }
        modified = findModifiedResources state
    assert $ not $ null modified

propFindModifiedResourcesIgnoresGet :: Property
propFindModifiedResourcesIgnoresGet = property $ do
    let getReq = mkRequest methodGet "/users/42"
        getRes = mkResponse 200
        state =
            emptyState
                { tsHistory = [("GET /users/42", getReq, getRes)]
                }
        modified = findModifiedResources state
    modified === []

-- Check name properties

propUseAfterFreeName :: Property
propUseAfterFreeName = property $ do
    scName useAfterFree === "use-after-free"

propEnsureResourceAvailabilityName :: Property
propEnsureResourceAvailabilityName = property $ do
    scName ensureResourceAvailability === "ensure-resource-availability"

propEnsureModificationPersistedName :: Property
propEnsureModificationPersistedName = property $ do
    scName ensureModificationPersisted === "ensure-modification-persisted"

-- StatefulCheckResult properties

propPassedHasCheckName :: Property
propPassedHasCheckName = property $ do
    let result =
            StatefulCheckPassed
                { scrCheckName = "test-check"
                , scrResource = Nothing
                }
    scrCheckName result === "test-check"

propFailedHasDetails :: Property
propFailedHasDetails = property $ do
    let resource = mkResource "/users/{id}" [("id", Number 42)]
        failure =
            StatefulFailure
                { sfCheckName = "test-check"
                , sfMessage = "Test failure message"
                , sfResource = resource
                , sfVerificationRequest = Nothing
                , sfVerificationResponse = Nothing
                , sfExpectedStatus = Just 404
                , sfActualStatus = Just 200
                }
        result = StatefulCheckFailed failure
    case result of
        StatefulCheckFailed f -> do
            sfCheckName f === "test-check"
            sfExpectedStatus f === Just 404
            sfActualStatus f === Just 200
        StatefulCheckPassed{} -> assert False

-- Helper functions

mkGetOp :: Text -> ResolvedOperation
mkGetOp path =
    ResolvedOperation
        { roMethod = "GET"
        , roPath = path
        , roOperationId = Nothing
        , roTags = []
        , roParameters = []
        , roRequestBody = Nothing
        , roResponses = Map.empty
        , roDefaultResponse = Nothing
        , roSecurity = []
        , roIsStreaming = False
        , roTimeout = Nothing
        }

mkResource :: Text -> [(Text, Value)] -> ResourceRef
mkResource path ids =
    ResourceRef
        { rrOperationId = "test-op"
        , rrResourcePath = path
        , rrIdentifiers = Map.fromList ids
        }

mkRequest :: Method -> Text -> ApiRequest
mkRequest method path =
    ApiRequest
        { reqMethod = method
        , reqPath = path
        , reqQueryParams = []
        , reqHeaders = []
        , reqBody = Nothing
        }

mkResponse :: Int -> ApiResponse
mkResponse status =
    ApiResponse
        { resStatusCode = status
        , resHeaders = []
        , resBody = ""
        , resTime = secondsToNominalDiffTime 0.1
        }
