{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for value extraction from API responses.
module Haskemathesis.Test.Properties.Stateful.Extract (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.Stateful.Extract
import Haskemathesis.Stateful.Types (TestState (..), ValueSource (..), emptyState)
import Haskemathesis.Test.Support (itProp)

tests :: Spec
tests = describe "Stateful.Extract" $ do
    describe "parseJsonPointer" $ do
        itProp "parses simple pointer" propParseSimplePointer
        itProp "parses nested pointer" propParseNestedPointer
        itProp "parses array index" propParseArrayIndex
        itProp "handles escape sequences" propParseEscapeSequences

    describe "parseOpenApiExpression" $ do
        itProp "parses $response.body# syntax" propParseOpenApiBody
        itProp "parses $. syntax" propParseDollarDot
        itProp "parses simple field name" propParseSimpleField

    describe "extractByPointer" $ do
        itProp "extracts from object" propExtractFromObject
        itProp "extracts nested field" propExtractNestedField
        itProp "extracts from array" propExtractFromArray
        itProp "returns Nothing for missing field" propExtractMissingField

    describe "extractByFieldName" $ do
        itProp "extracts existing field" propExtractFieldExists
        itProp "returns Nothing for non-object" propExtractFieldNonObject

    describe "extractFromHeader" $ do
        itProp "extracts header value" propExtractHeader
        itProp "is case insensitive" propExtractHeaderCaseInsensitive
        itProp "returns Nothing for missing header" propExtractHeaderMissing

    describe "extractFromLocation" $ do
        itProp "extracts Location header" propExtractLocation

    describe "extractValue" $ do
        itProp "handles FromResponseBody" propExtractValueBody
        itProp "handles FromResponseHeader" propExtractValueHeader
        itProp "handles FromState" propExtractValueState
        itProp "handles Literal" propExtractValueLiteral

    describe "extractIdFields" $ do
        itProp "extracts id field" propExtractIdField
        itProp "extracts fields ending in Id" propExtractFieldsEndingId
        itProp "ignores complex values" propIgnoreComplexValues

-- parseJsonPointer properties

propParseSimplePointer :: Property
propParseSimplePointer = property $ do
    parseJsonPointer "/id" === Just ["id"]

propParseNestedPointer :: Property
propParseNestedPointer = property $ do
    parseJsonPointer "/user/profile/name" === Just ["user", "profile", "name"]

propParseArrayIndex :: Property
propParseArrayIndex = property $ do
    parseJsonPointer "/data/0/id" === Just ["data", "0", "id"]

propParseEscapeSequences :: Property
propParseEscapeSequences = property $ do
    -- ~0 -> ~, ~1 -> /
    parseJsonPointer "/field~0name" === Just ["field~name"]
    parseJsonPointer "/field~1name" === Just ["field/name"]

-- parseOpenApiExpression properties

propParseOpenApiBody :: Property
propParseOpenApiBody = property $ do
    parseOpenApiExpression "$response.body#/id" === Just ["id"]
    parseOpenApiExpression "$response.body#/user/id" === Just ["user", "id"]

propParseDollarDot :: Property
propParseDollarDot = property $ do
    parseOpenApiExpression "$.id" === Just ["id"]
    parseOpenApiExpression "$.user.id" === Just ["user", "id"]

propParseSimpleField :: Property
propParseSimpleField = property $ do
    parseOpenApiExpression "id" === Just ["id"]

-- extractByPointer properties

propExtractFromObject :: Property
propExtractFromObject = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let obj = Object (KeyMap.singleton "id" (Number (fromIntegral n)))
    extractByPointer ["id"] obj === Just (Number (fromIntegral n))

propExtractNestedField :: Property
propExtractNestedField = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let nested = Object (KeyMap.singleton "name" (String txt))
        obj = Object (KeyMap.singleton "user" nested)
    extractByPointer ["user", "name"] obj === Just (String txt)

propExtractFromArray :: Property
propExtractFromArray = property $ do
    let arr = Array (V.fromList [Number 10, Number 20, Number 30])
    extractByPointer ["1"] arr === Just (Number 20)

propExtractMissingField :: Property
propExtractMissingField = property $ do
    let obj = Object (KeyMap.singleton "id" (Number 42))
    extractByPointer ["nonexistent"] obj === Nothing

-- extractByFieldName properties

propExtractFieldExists :: Property
propExtractFieldExists = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let obj = Object (KeyMap.singleton "id" (Number (fromIntegral n)))
    extractByFieldName "id" obj === Just (Number (fromIntegral n))

propExtractFieldNonObject :: Property
propExtractFieldNonObject = property $ do
    extractByFieldName "id" (String "not an object") === Nothing
    extractByFieldName "id" (Number 42) === Nothing
    extractByFieldName "id" (Array V.empty) === Nothing

-- extractFromHeader properties

propExtractHeader :: Property
propExtractHeader = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let response = ApiResponse 200 [("X-Request-Id", encodeUtf8 txt)] "" 0.1
    extractFromHeader "X-Request-Id" response === Just (String txt)

propExtractHeaderCaseInsensitive :: Property
propExtractHeaderCaseInsensitive = property $ do
    let response = ApiResponse 200 [("Content-Type", "application/json")] "" 0.1
    extractFromHeader "content-type" response === Just (String "application/json")
    extractFromHeader "CONTENT-TYPE" response === Just (String "application/json")

propExtractHeaderMissing :: Property
propExtractHeaderMissing = property $ do
    let response = ApiResponse 200 [] "" 0.1
    extractFromHeader "X-Missing" response === Nothing

-- extractFromLocation properties

propExtractLocation :: Property
propExtractLocation = property $ do
    path <- forAll $ Gen.text (Range.linear 5 30) Gen.alphaNum
    let location = "/users/" <> path
        response = ApiResponse 201 [("Location", encodeUtf8 location)] "" 0.1
    extractFromLocation response === Just location

-- extractValue properties

propExtractValueBody :: Property
propExtractValueBody = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let source = FromResponseBody "$response.body#/id"
        body = "{\"id\": " <> encodeUtf8 (T.pack (show n)) <> "}"
        response = ApiResponse 200 [] body 0.1
    extractValue source emptyState response === Just (Number (fromIntegral n))

propExtractValueHeader :: Property
propExtractValueHeader = property $ do
    let source = FromResponseHeader "X-Token"
        response = ApiResponse 200 [("X-Token", "abc123")] "" 0.1
    extractValue source emptyState response === Just (String "abc123")

propExtractValueState :: Property
propExtractValueState = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let source = FromState "userId"
        state = emptyState{tsExtractedValues = Map.singleton "userId" (Number (fromIntegral n))}
        response = ApiResponse 200 [] "" 0.1
    extractValue source state response === Just (Number (fromIntegral n))

propExtractValueLiteral :: Property
propExtractValueLiteral = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let source = Literal (String txt)
        response = ApiResponse 200 [] "" 0.1
    extractValue source emptyState response === Just (String txt)

-- extractIdFields properties

propExtractIdField :: Property
propExtractIdField = property $ do
    n <- forAll $ Gen.int (Range.linear 0 1000)
    let obj = KeyMap.singleton "id" (Number (fromIntegral n))
        result = extractIdFields obj
    Map.lookup "id" result === Just (Number (fromIntegral n))

propExtractFieldsEndingId :: Property
propExtractFieldsEndingId = property $ do
    let obj =
            KeyMap.fromList
                [ ("userId", Number 1)
                , ("groupId", Number 2)
                , ("user_id", Number 3)
                ]
        result = extractIdFields obj
    assert $ Map.size result == 3
    Map.lookup "userId" result === Just (Number 1)
    Map.lookup "groupId" result === Just (Number 2)
    Map.lookup "user_id" result === Just (Number 3)

propIgnoreComplexValues :: Property
propIgnoreComplexValues = property $ do
    let obj =
            KeyMap.fromList
                [ ("id", Number 42)
                , ("nestedId", Object KeyMap.empty) -- Complex value, should be ignored
                , ("arrayId", Array V.empty) -- Complex value, should be ignored
                ]
        result = extractIdFields obj
    -- Only "id" should be extracted (simple value)
    Map.size result === 1
    Map.lookup "id" result === Just (Number 42)
