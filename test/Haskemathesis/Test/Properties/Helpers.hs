{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for check helper functions.

These tests verify that helper functions correctly:

1. Generate operation labels
2. Normalize media types
3. Match content types
4. Handle JSON suffix matching
-}
module Haskemathesis.Test.Properties.Helpers (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Standard.Helpers (
    jsonSuffixMatch,
    matchesContentType,
    normalizeMediaType,
    operationLabel,
 )
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Schema (emptySchema)
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "Check Helpers" $ do
        describe "operationLabel" $ do
            itProp "uses operationId when present" propLabelUsesOperationId
            itProp "falls back to method+path when no operationId" propLabelFallsBackToMethodPath

        describe "normalizeMediaType" $ do
            itProp "strips charset parameter" propNormalizeStripsCharset
            itProp "converts to lowercase" propNormalizeToLowercase
            itProp "strips leading/trailing whitespace" propNormalizeStripsWhitespace
            itProp "handles complex media types" propNormalizeComplexMediaTypes

        describe "matchesContentType" $ do
            itProp "matches exact content type" propMatchesExactContentType
            itProp "matches with charset parameter" propMatchesWithCharset
            itProp "handles case insensitivity" propMatchesCaseInsensitive

        describe "jsonSuffixMatch" $ do
            itProp "matches +json suffix to application/json" propJsonSuffixMatchesAppJson
            itProp "matches application/json to +json suffix" propAppJsonMatchesSuffix
            itProp "does not match non-json types" propJsonSuffixNoMatchNonJson

-- Generators

genOperationId :: Gen Text
genOperationId = Gen.text (Range.linear 3 20) Gen.alphaNum

genMethod :: Gen Text
genMethod = Gen.element ["GET", "POST", "PUT", "DELETE", "PATCH"]

genPath :: Gen Text
genPath = do
    segments <- Gen.list (Range.linear 1 4) (Gen.text (Range.linear 1 10) Gen.alphaNum)
    pure $ "/" <> T.intercalate "/" segments

-- operationLabel properties

propLabelUsesOperationId :: Property
propLabelUsesOperationId = property $ do
    opId <- forAll genOperationId
    method <- forAll genMethod
    path <- forAll genPath
    let op = emptyOperation{roOperationId = Just opId, roMethod = method, roPath = path}
    operationLabel op === opId

propLabelFallsBackToMethodPath :: Property
propLabelFallsBackToMethodPath = property $ do
    method <- forAll genMethod
    path <- forAll genPath
    let op = emptyOperation{roOperationId = Nothing, roMethod = method, roPath = path}
    operationLabel op === (method <> " " <> path)

-- normalizeMediaType properties

propNormalizeStripsCharset :: Property
propNormalizeStripsCharset = property $ do
    baseType <- forAll $ Gen.element ["application/json", "text/html", "application/xml"]
    charset <- forAll $ Gen.element ["utf-8", "UTF-8", "iso-8859-1"]
    let withCharset = baseType <> "; charset=" <> charset
    normalizeMediaType withCharset === baseType

propNormalizeToLowercase :: Property
propNormalizeToLowercase = property $ do
    let upperCase = "APPLICATION/JSON"
    normalizeMediaType upperCase === "application/json"

propNormalizeStripsWhitespace :: Property
propNormalizeStripsWhitespace = property $ do
    baseType <- forAll $ Gen.element ["application/json", "text/html"]
    let withSpaces = "  " <> baseType <> "  "
    normalizeMediaType withSpaces === baseType

propNormalizeComplexMediaTypes :: Property
propNormalizeComplexMediaTypes = property $ do
    let complex = "application/json; charset=utf-8; boundary=something"
    normalizeMediaType complex === "application/json"

-- matchesContentType properties

propMatchesExactContentType :: Property
propMatchesExactContentType = property $ do
    contentType <- forAll $ Gen.element ["application/json", "text/html", "application/xml"]
    let schemas = Map.singleton contentType emptySchema
    assert $ matchesContentType contentType schemas

propMatchesWithCharset :: Property
propMatchesWithCharset = property $ do
    baseType <- forAll $ Gen.element ["application/json", "text/html"]
    charset <- forAll $ Gen.element ["utf-8", "UTF-8"]
    let responseType = baseType <> "; charset=" <> charset
        schemas = Map.singleton baseType emptySchema
    assert $ matchesContentType responseType schemas

propMatchesCaseInsensitive :: Property
propMatchesCaseInsensitive = property $ do
    let responseType = "APPLICATION/JSON"
        schemas = Map.singleton "application/json" emptySchema
    assert $ matchesContentType responseType schemas

-- jsonSuffixMatch properties

propJsonSuffixMatchesAppJson :: Property
propJsonSuffixMatchesAppJson = property $ do
    vendor <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    let responseType = "application/vnd." <> vendor <> "+json"
        declaredTypes = ["application/json"]
    assert $ jsonSuffixMatch responseType declaredTypes

propAppJsonMatchesSuffix :: Property
propAppJsonMatchesSuffix = property $ do
    vendor <- forAll $ Gen.text (Range.linear 1 10) Gen.alpha
    let responseType = "application/json"
        declaredTypes = ["application/vnd." <> vendor <> "+json"]
    assert $ jsonSuffixMatch responseType declaredTypes

propJsonSuffixNoMatchNonJson :: Property
propJsonSuffixNoMatchNonJson = property $ do
    let responseType = "text/html"
        declaredTypes = ["application/json"]
    assert $ not (jsonSuffixMatch responseType declaredTypes)
