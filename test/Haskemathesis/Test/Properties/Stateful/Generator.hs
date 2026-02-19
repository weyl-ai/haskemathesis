{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for state-aware request generation.
module Haskemathesis.Test.Properties.Stateful.Generator (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
 )
import Haskemathesis.Schema (emptySchema)
import Haskemathesis.Stateful.Generator
import Haskemathesis.Stateful.Types (
    OperationLink (..),
    ParameterBinding (..),
    SequenceStep (..),
    TestState (..),
    ValueSource (..),
    emptyState,
 )
import Haskemathesis.Test.Support (itProp)

tests :: Spec
tests = describe "Stateful.Generator" $ do
    describe "resolveBinding" $ do
        itProp "resolves FromState from state" propResolveFromState
        itProp "resolves Literal values" propResolveLiteral
        itProp "returns Nothing for missing state key" propResolveMissingState
        itProp "returns Nothing for FromResponseBody" propResolveResponseBody
        itProp "returns Nothing for FromResponseHeader" propResolveResponseHeader

    describe "fillParamFromState" $ do
        itProp "finds existing value" propFillFromStateExists
        itProp "returns Nothing for missing value" propFillFromStateMissing

    describe "renderValueForParam" $ do
        itProp "renders strings without quotes" propRenderString
        itProp "renders numbers as text" propRenderNumber
        itProp "renders booleans as lowercase" propRenderBool
        itProp "renders null as 'null'" propRenderNull
        itProp "renders arrays as JSON" propRenderArray
        itProp "renders objects as JSON" propRenderObject

    describe "updateState" $ do
        itProp "adds request/response to history" propUpdateHistory
        itProp "extracts id fields from response" propUpdateExtractsId
        itProp "extracts userId field from response" propUpdateExtractsUserId
        itProp "tracks created resources for POST" propUpdateTracksResource
        itProp "does not track resources for GET" propUpdateNoTrackGet
        itProp "preserves existing state values" propUpdatePreserves

    describe "getRequiredBindings" $ do
        itProp "returns path params" propRequiredPathParams
        itProp "excludes query params" propRequiredExcludesQuery
        itProp "excludes optional params" propRequiredExcludesOptional

    describe "applyBindings" $ do
        itProp "creates step with literal bindings" propApplyLiteralBindings
        itProp "resolves response body bindings" propApplyResponseBodyBindings

-- resolveBinding properties

propResolveFromState :: Property
propResolveFromState = property $ do
    key <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    value <- forAll genSimpleValue
    let state = emptyState{tsExtractedValues = Map.singleton key value}
        source = FromState key
    resolveBinding source state === Just value

propResolveLiteral :: Property
propResolveLiteral = property $ do
    value <- forAll genSimpleValue
    let source = Literal value
    resolveBinding source emptyState === Just value

propResolveMissingState :: Property
propResolveMissingState = property $ do
    let state = emptyState
        source = FromState "nonexistent"
    resolveBinding source state === Nothing

propResolveResponseBody :: Property
propResolveResponseBody = property $ do
    let source = FromResponseBody "$response.body#/id"
    resolveBinding source emptyState === Nothing

propResolveResponseHeader :: Property
propResolveResponseHeader = property $ do
    let source = FromResponseHeader "Location"
    resolveBinding source emptyState === Nothing

-- fillParamFromState properties

propFillFromStateExists :: Property
propFillFromStateExists = property $ do
    paramName <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    value <- forAll genSimpleValue
    let state = emptyState{tsExtractedValues = Map.singleton paramName value}
    fillParamFromState state paramName === Just value

propFillFromStateMissing :: Property
propFillFromStateMissing = property $ do
    let state = emptyState
    fillParamFromState state "missing" === Nothing

-- renderValueForParam properties

propRenderString :: Property
propRenderString = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    renderValueForParam (String txt) === txt

propRenderNumber :: Property
propRenderNumber = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let rendered = renderValueForParam (Number (fromIntegral n))
    -- Should be numeric string, not JSON
    assert $ not $ T.isPrefixOf "\"" rendered

propRenderBool :: Property
propRenderBool = property $ do
    renderValueForParam (Bool True) === "true"
    renderValueForParam (Bool False) === "false"

propRenderNull :: Property
propRenderNull = property $ do
    renderValueForParam Null === "null"

propRenderArray :: Property
propRenderArray = property $ do
    let arr = Array mempty
        rendered = renderValueForParam arr
    rendered === "[]"

propRenderObject :: Property
propRenderObject = property $ do
    let obj = Object mempty
        rendered = renderValueForParam obj
    rendered === "{}"

-- updateState properties

propUpdateHistory :: Property
propUpdateHistory = property $ do
    let state = emptyState
        op = mkOp "POST" "/users" (Just "createUser") []
        req = mkRequest "POST" "/users"
        res = mkResponse 201 "{\"id\": 42}"
        newState = updateState "createUser" op req res state
    length (tsHistory newState) === 1

propUpdateExtractsId :: Property
propUpdateExtractsId = property $ do
    let state = emptyState
        op = mkOp "POST" "/users" (Just "createUser") []
        req = mkRequest "POST" "/users"
        res = mkResponse 201 "{\"id\": 42, \"name\": \"Alice\"}"
        newState = updateState "createUser" op req res state
    -- Should have extracted "id"
    case Map.lookup "id" (tsExtractedValues newState) of
        Just (Number n) -> n === 42
        _ -> assert False

propUpdateExtractsUserId :: Property
propUpdateExtractsUserId = property $ do
    let state = emptyState
        op = mkOp "POST" "/users" (Just "createUser") []
        req = mkRequest "POST" "/users"
        res = mkResponse 201 "{\"userId\": 123}"
        newState = updateState "createUser" op req res state
    -- Should have extracted "userId" (ends with Id)
    case Map.lookup "userId" (tsExtractedValues newState) of
        Just (Number n) -> n === 123
        _ -> assert False

propUpdateTracksResource :: Property
propUpdateTracksResource = property $ do
    let state = emptyState
        op = mkOp "POST" "/users" (Just "createUser") []
        req = mkRequest "POST" "/users"
        res = mkResponse 201 "{\"id\": 42}"
        newState = updateState "createUser" op req res state
    -- POST with 2xx should track resource
    length (tsCreatedResources newState) === 1

propUpdateNoTrackGet :: Property
propUpdateNoTrackGet = property $ do
    let state = emptyState
        op = mkOp "GET" "/users/42" (Just "getUser") [mkPathParam "id"]
        req = mkRequest "GET" "/users/42"
        res = mkResponse 200 "{\"id\": 42}"
        newState = updateState "getUser" op req res state
    -- GET should not track resources
    tsCreatedResources newState === []

propUpdatePreserves :: Property
propUpdatePreserves = property $ do
    let existingValue = Number 999
        state = emptyState{tsExtractedValues = Map.singleton "existing" existingValue}
        op = mkOp "POST" "/users" (Just "createUser") []
        req = mkRequest "POST" "/users"
        res = mkResponse 201 "{\"id\": 42}"
        newState = updateState "createUser" op req res state
    -- Should preserve existing values
    Map.lookup "existing" (tsExtractedValues newState) === Just existingValue

-- getRequiredBindings properties

propRequiredPathParams :: Property
propRequiredPathParams = property $ do
    let op = mkOp "GET" "/users/{userId}" (Just "getUser") [mkPathParam "userId"]
        bindings = getRequiredBindings op
    bindings === ["userId"]

propRequiredExcludesQuery :: Property
propRequiredExcludesQuery = property $ do
    let queryParam = ResolvedParam "page" ParamQuery True emptySchema
        pathParam = mkPathParam "id"
        op = mkOp "GET" "/users/{id}" (Just "getUsers") [pathParam, queryParam]
        bindings = getRequiredBindings op
    -- Should only include path param
    bindings === ["id"]

propRequiredExcludesOptional :: Property
propRequiredExcludesOptional = property $ do
    let optionalParam = ResolvedParam "format" ParamPath False emptySchema
        requiredParam = mkPathParam "id"
        op = mkOp "GET" "/users/{id}" (Just "getUser") [requiredParam, optionalParam]
        bindings = getRequiredBindings op
    -- Should only include required path param
    bindings === ["id"]

-- applyBindings properties

propApplyLiteralBindings :: Property
propApplyLiteralBindings = property $ do
    let link =
            OperationLink
                { olSourceOperation = "createUser"
                , olTargetOperation = "getUser"
                , olParameterBindings = [ParameterBinding "id" (Literal (Number 42))]
                , olDescription = Nothing
                , olLinkName = Nothing
                }
        res = mkResponse 201 "{}"
        step = applyBindings link emptyState res
    ssOperationId step === "getUser"
    case Map.lookup "id" (ssParamBindings step) of
        Just (Literal (Number n)) -> n === 42
        _ -> assert False

propApplyResponseBodyBindings :: Property
propApplyResponseBodyBindings = property $ do
    let link =
            OperationLink
                { olSourceOperation = "createUser"
                , olTargetOperation = "getUser"
                , olParameterBindings = [ParameterBinding "id" (FromResponseBody "$response.body#/id")]
                , olDescription = Nothing
                , olLinkName = Nothing
                }
        res = mkResponse 201 "{\"id\": 99}"
        step = applyBindings link emptyState res
    -- Should resolve to literal value
    case Map.lookup "id" (ssParamBindings step) of
        Just (Literal (Number n)) -> n === 99
        _ -> assert False

-- Helper functions

genSimpleValue :: Gen Value
genSimpleValue =
    Gen.choice
        [ String <$> Gen.text (Range.linear 1 10) Gen.alphaNum
        , Number . fromIntegral <$> Gen.int (Range.linear 0 1000)
        , Bool <$> Gen.bool
        ]

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

mkRequest :: Text -> Text -> ApiRequest
mkRequest method path =
    ApiRequest
        { reqMethod = encodeUtf8 method
        , reqPath = path
        , reqQueryParams = []
        , reqHeaders = []
        , reqBody = Nothing
        }
  where
    encodeUtf8 = T.encodeUtf8

mkResponse :: Int -> BS.ByteString -> ApiResponse
mkResponse code body =
    ApiResponse
        { resStatusCode = code
        , resHeaders = []
        , resBody = body
        , resTime = 0.1
        }
