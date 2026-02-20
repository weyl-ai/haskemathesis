{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for heuristic link detection.
module Haskemathesis.Test.Properties.Stateful.Heuristics (
    tests,
) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
 )
import Haskemathesis.Schema (emptySchema)
import Haskemathesis.Stateful.Heuristics
import Haskemathesis.Stateful.Types (OperationLink (..), ParameterBinding (..), ValueSource (..))
import Haskemathesis.Test.Support (itProp)

tests :: Spec
tests = describe "Stateful.Heuristics" $ do
    describe "extractBasePath" $ do
        itProp "extracts base from /resource/{id}" propBasePathSimple
        itProp "extracts base from nested path" propBasePathNested
        itProp "returns same path when no params" propBasePathNoParams
        itProp "handles multiple trailing params" propBasePathMultipleTrailing

    describe "extractPathParams" $ do
        itProp "extracts single param" propExtractSingleParam
        itProp "extracts multiple params" propExtractMultipleParams
        itProp "returns empty for no params" propExtractNoParams
        itProp "handles mixed segments" propExtractMixedSegments

    describe "normalizeParamName" $ do
        itProp "lowercases names" propNormalizeLowercase
        itProp "removes underscores" propNormalizeNoUnderscores
        itProp "converts camelCase" propNormalizeCamelCase

    describe "namesMatch" $ do
        itProp "matches exact names" propNamesMatchExact
        itProp "matches userId to user_id" propNamesMatchCamelSnake
        itProp "matches field ending in id to id param" propNamesMatchIdSuffix
        itProp "does not match unrelated names" propNamesNoMatch

    describe "findRelatedOperations" $ do
        itProp "finds GET for POST collection" propFindRelatedGet
        itProp "finds PUT and DELETE for POST" propFindRelatedMultiple
        itProp "does not find unrelated paths" propFindRelatedUnrelated

    describe "inferLinksFromPaths" $ do
        itProp "creates link from POST to GET" propInferLinkPostToGet
        itProp "binds path params from response" propInferLinkBindings
        itProp "handles nested resources" propInferLinkNested

    describe "matchResponseFieldsToParams" $ do
        itProp "matches id field to id param" propMatchFieldsId
        itProp "matches with naming normalization" propMatchFieldsNormalized
        itProp "returns empty for no matches" propMatchFieldsNoMatch

    describe "inferLinks" $ do
        itProp "combines path and schema inference" propInferLinksCombined

    describe "edge cases" $ do
        itProp "handles empty path" propEmptyPath
        itProp "handles root path" propRootPath
        itProp "handles path with no segments" propPathNoSegments
        itProp "finds PATCH operations" propFindRelatedPatch
        itProp "excludes self from related" propExcludesSelf
        itProp "handles operations without operationId" propNoOperationId
        itProp "normalizes mixed case names" propNormalizeMixedCase
        itProp "matches multiple params in path" propMatchMultipleParams

-- extractBasePath properties

propBasePathSimple :: Property
propBasePathSimple = property $ do
    let path = "/users/{id}"
        result = extractBasePath path
    result === "/users"

propBasePathNested :: Property
propBasePathNested = property $ do
    let path = "/users/{userId}/posts/{postId}"
        result = extractBasePath path
    result === "/users/{userId}/posts"

propBasePathNoParams :: Property
propBasePathNoParams = property $ do
    segment <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    let path = "/" <> segment
        result = extractBasePath path
    result === path

propBasePathMultipleTrailing :: Property
propBasePathMultipleTrailing = property $ do
    -- Path like /a/{b}/{c} should extract to /a
    let path = "/resources/{id}/{subId}"
        result = extractBasePath path
    result === "/resources"

-- extractPathParams properties

propExtractSingleParam :: Property
propExtractSingleParam = property $ do
    paramName <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    let path = "/resource/{" <> paramName <> "}"
        result = extractPathParams path
    result === [paramName]

propExtractMultipleParams :: Property
propExtractMultipleParams = property $ do
    let path = "/users/{userId}/posts/{postId}"
        result = extractPathParams path
    result === ["userId", "postId"]

propExtractNoParams :: Property
propExtractNoParams = property $ do
    let path = "/users/posts/comments"
        result = extractPathParams path
    result === []

propExtractMixedSegments :: Property
propExtractMixedSegments = property $ do
    let path = "/api/v1/{orgId}/users/{userId}"
        result = extractPathParams path
    result === ["orgId", "userId"]

-- normalizeParamName properties

propNormalizeLowercase :: Property
propNormalizeLowercase = property $ do
    let result = normalizeParamName "UserID"
    -- Should be lowercase
    assert $ T.toLower result == result

propNormalizeNoUnderscores :: Property
propNormalizeNoUnderscores = property $ do
    let result = normalizeParamName "user_id"
    -- Should have no underscores
    assert $ not $ T.any (== '_') result

propNormalizeCamelCase :: Property
propNormalizeCamelCase = property $ do
    -- userId and user_id should normalize to same value
    let camel = normalizeParamName "userId"
        snake = normalizeParamName "user_id"
    camel === snake

-- namesMatch properties

propNamesMatchExact :: Property
propNamesMatchExact = property $ do
    name <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    assert $ namesMatch name name

propNamesMatchCamelSnake :: Property
propNamesMatchCamelSnake = property $ do
    assert $ namesMatch "userId" "user_id"
    assert $ namesMatch "postId" "post_id"
    assert $ namesMatch "resourceName" "resource_name"

propNamesMatchIdSuffix :: Property
propNamesMatchIdSuffix = property $ do
    -- Fields ending in "id" should match "id" param
    assert $ namesMatch "userId" "id"
    assert $ namesMatch "postId" "id"
    assert $ namesMatch "resourceId" "id"

propNamesNoMatch :: Property
propNamesNoMatch = property $ do
    assert $ not $ namesMatch "name" "id"
    assert $ not $ namesMatch "email" "userId"
    assert $ not $ namesMatch "title" "postId"

-- findRelatedOperations properties

propFindRelatedGet :: Property
propFindRelatedGet = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        allOps = [postOp, getOp]
        related = findRelatedOperations allOps postOp
    case related of
        [op] -> roOperationId op === Just "getUser"
        [] -> assert False
        _multipleOps -> assert False

propFindRelatedMultiple :: Property
propFindRelatedMultiple = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        putOp = mkOp "PUT" "/users/{id}" (Just "updateUser") [mkPathParam "id"]
        deleteOp = mkOp "DELETE" "/users/{id}" (Just "deleteUser") [mkPathParam "id"]
        allOps = [postOp, getOp, putOp, deleteOp]
        related = findRelatedOperations allOps postOp
    -- Verify exactly 3 related operations using pattern matching
    case related of
        [_, _, _] -> pure ()
        _other -> assert False

propFindRelatedUnrelated :: Property
propFindRelatedUnrelated = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        postsOp = mkOp "GET" "/posts/{id}" (Just "getPost") [mkPathParam "id"]
        allOps = [postOp, postsOp]
        related = findRelatedOperations allOps postOp
    related === []

-- inferLinksFromPaths properties

propInferLinkPostToGet :: Property
propInferLinkPostToGet = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        links = inferLinksFromPaths [postOp, getOp]
    case links of
        [link] -> do
            olSourceOperation link === "createUser"
            olTargetOperation link === "getUser"
        [] -> assert False
        _multipleLinks -> assert False

propInferLinkBindings :: Property
propInferLinkBindings = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{userId}" (Just "getUser") [mkPathParam "userId"]
        links = inferLinksFromPaths [postOp, getOp]
    case links of
        [link] -> case olParameterBindings link of
            [binding] -> do
                pbTargetParam binding === "userId"
                case pbSource binding of
                    FromResponseBody path -> assert $ T.isInfixOf "userId" path
                    Literal{} -> assert False
                    FromState{} -> assert False
                    FromResponseHeader{} -> assert False
            [] -> assert False
            _multipleBindings -> assert False
        [] -> assert False
        _multipleLinks -> assert False

propInferLinkNested :: Property
propInferLinkNested = property $ do
    let postOp = mkOp "POST" "/users/{userId}/posts" (Just "createPost") [mkPathParam "userId"]
        getOp = mkOp "GET" "/users/{userId}/posts/{postId}" (Just "getPost") [mkPathParam "userId", mkPathParam "postId"]
        links = inferLinksFromPaths [postOp, getOp]
    -- Should only bind postId (userId is already in both paths)
    case links of
        [link] -> case olParameterBindings link of
            [binding] -> pbTargetParam binding === "postId"
            [] -> assert False
            _multipleBindings -> assert False
        [] -> assert False
        _multipleLinks -> assert False

-- matchResponseFieldsToParams properties

propMatchFieldsId :: Property
propMatchFieldsId = property $ do
    let fields = ["id", "name", "email"]
        params = ["id"]
        matches = matchResponseFieldsToParams fields params
    matches === [("id", "id")]

propMatchFieldsNormalized :: Property
propMatchFieldsNormalized = property $ do
    let fields = ["userId", "userName"]
        params = ["user_id"]
        matches = matchResponseFieldsToParams fields params
    -- Should match userId to user_id
    case matches of
        [(field, param)] -> do
            field === "userId"
            param === "user_id"
        [] -> assert False
        _multipleMatches -> assert False

propMatchFieldsNoMatch :: Property
propMatchFieldsNoMatch = property $ do
    let fields = ["name", "email"]
        params = ["id"]
        matches = matchResponseFieldsToParams fields params
    matches === []

-- inferLinks properties

propInferLinksCombined :: Property
propInferLinksCombined = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        deleteOp = mkOp "DELETE" "/users/{id}" (Just "deleteUser") [mkPathParam "id"]
        links = inferLinks [postOp, getOp, deleteOp]
    -- Should have links from POST to both GET and DELETE (at least 2)
    case links of
        (_ : _ : _) -> pure () -- At least 2 elements
        _fewer -> assert False

-- Helper functions

mkOp :: Text -> Text -> Maybe Text -> [ResolvedParam] -> ResolvedOperation
mkOp method path opId params =
    ResolvedOperation
        { roMethod = method
        , roPath = path
        , roOperationId = opId
        , roTags = []
        , roParameters = params
        , roRequestBody = Nothing
        , roResponses = Map.empty
        , roDefaultResponse = Nothing
        , roSecurity = []
        , roIsStreaming = False
        , roTimeout = Nothing
        }

mkPathParam :: Text -> ResolvedParam
mkPathParam name =
    ResolvedParam
        { rpName = name
        , rpLocation = ParamPath
        , rpRequired = True
        , rpSchema = emptySchema
        }

-- Edge case properties

propEmptyPath :: Property
propEmptyPath = property $ do
    let result = extractBasePath ""
    result === ""

propRootPath :: Property
propRootPath = property $ do
    let result = extractBasePath "/"
    result === "/"

propPathNoSegments :: Property
propPathNoSegments = property $ do
    let result = extractPathParams "/"
    result === []

propFindRelatedPatch :: Property
propFindRelatedPatch = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        patchOp = mkOp "PATCH" "/users/{id}" (Just "patchUser") [mkPathParam "id"]
        allOps = [postOp, patchOp]
        related = findRelatedOperations allOps postOp
    case related of
        [op] -> roOperationId op === Just "patchUser"
        [] -> assert False
        _multipleOps -> assert False

propExcludesSelf :: Property
propExcludesSelf = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        allOps = [postOp]
        related = findRelatedOperations allOps postOp
    -- Should not include itself
    related === []

propNoOperationId :: Property
propNoOperationId = property $ do
    let postOp = mkOp "POST" "/users" Nothing []
        getOp = mkOp "GET" "/users/{id}" Nothing [mkPathParam "id"]
        links = inferLinksFromPaths [postOp, getOp]
    -- Should still work with fallback labels
    case links of
        [link] -> do
            olSourceOperation link === "POST /users"
            olTargetOperation link === "GET /users/{id}"
        [] -> assert False
        _multipleLinks -> assert False

propNormalizeMixedCase :: Property
propNormalizeMixedCase = property $ do
    -- All of these should normalize to the same value
    let n1 = normalizeParamName "userId"
        n2 = normalizeParamName "UserId"
        n3 = normalizeParamName "USERID"
        n4 = normalizeParamName "user_id"
    -- They should all equal each other
    assert $ n1 == n2
    assert $ n2 == n3
    -- Note: USERID normalizes differently because it's all caps
    -- but userId and user_id should match
    assert $ n1 == n4

propMatchMultipleParams :: Property
propMatchMultipleParams = property $ do
    let postOp = mkOp "POST" "/orgs/{orgId}/users" (Just "createUser") [mkPathParam "orgId"]
        getOp = mkOp "GET" "/orgs/{orgId}/users/{userId}" (Just "getUser") [mkPathParam "orgId", mkPathParam "userId"]
        links = inferLinksFromPaths [postOp, getOp]
    -- Should create link with only new param (userId)
    case links of
        [link] -> do
            -- Should only have one binding (for userId, not orgId)
            case olParameterBindings link of
                [binding] -> pbTargetParam binding === "userId"
                [] -> assert False
                _multipleBindings -> assert False
        [] -> assert False
        _multipleLinks -> assert False
