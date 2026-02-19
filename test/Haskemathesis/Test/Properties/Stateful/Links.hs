{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for OpenAPI Links parsing and resolution.
module Haskemathesis.Test.Properties.Stateful.Links (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.Text as T
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Stateful.Links
import Haskemathesis.Stateful.Types (ValueSource (..))
import Haskemathesis.Test.Support (itProp)

tests :: Spec
tests = describe "Stateful.Links" $ do
    describe "parseExpression" $ do
        itProp "parses $response.body# syntax" propParseResponseBody
        itProp "parses $response.header. syntax" propParseResponseHeader
        itProp "parses $request.path. syntax" propParseRequestPath
        itProp "parses $request.query. syntax" propParseRequestQuery
        itProp "parses $url expression" propParseUrl
        itProp "treats plain text as literal" propParsePlainText
        itProp "handles nested JSON pointers" propParseNestedPointer
        itProp "handles $request.body# syntax" propParseRequestBody

    describe "parseRuntimeExpression" $ do
        itProp "parses response body expressions" propRuntimeResponseBody
        itProp "parses response header expressions" propRuntimeResponseHeader
        itProp "rejects non-expressions" propRuntimeNonExpression
        itProp "rejects unsupported expressions" propRuntimeUnsupported
        itProp "parses request path expressions" propRuntimeRequestPath
        itProp "parses url expression" propRuntimeUrl

    describe "decodeJsonPointer" $ do
        itProp "decodes ~1 to /" propDecodeSlash
        itProp "decodes ~0 to ~" propDecodeTilde
        itProp "handles mixed escape sequences" propDecodeMixed
        itProp "preserves plain text" propDecodePreserve
        itProp "handles empty string" propDecodeEmpty

    describe "parseOperationRef" $ do
        itProp "parses valid operation ref" propParseOpRefValid
        itProp "returns Nothing for non-path ref" propParseOpRefInvalid
        itProp "returns Nothing for Nothing" propParseOpRefNothing
        itProp "decodes encoded paths" propParseOpRefEncoded

-- parseExpression properties

propParseResponseBody :: Property
propParseResponseBody = property $ do
    field <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$response.body#/" <> field
        result = parseExpression expr
    case result of
        FromResponseBody path -> path === expr
        _other -> assert False

propParseResponseHeader :: Property
propParseResponseHeader = property $ do
    header <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$response.header." <> header
        result = parseExpression expr
    case result of
        FromResponseHeader h -> h === header
        _other -> assert False

propParseRequestPath :: Property
propParseRequestPath = property $ do
    param <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$request.path." <> param
        result = parseExpression expr
    case result of
        FromState key -> key === ("path." <> param)
        _other -> assert False

propParseRequestQuery :: Property
propParseRequestQuery = property $ do
    param <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$request.query." <> param
        result = parseExpression expr
    case result of
        FromState key -> key === ("query." <> param)
        _other -> assert False

propParseUrl :: Property
propParseUrl = property $ do
    let result = parseExpression "$url"
    case result of
        FromState key -> key === "url"
        _other -> assert False

propParsePlainText :: Property
propParsePlainText = property $ do
    -- Generate text that doesn't start with $
    txt <- forAll $ do
        first <- Gen.alpha
        rest <- Gen.text (Range.linear 0 20) Gen.alphaNum
        pure $ T.cons first rest
    let result = parseExpression txt
    case result of
        Literal (String s) -> s === txt
        _other -> assert False

propParseNestedPointer :: Property
propParseNestedPointer = property $ do
    let expr = "$response.body#/data/items/0/id"
        result = parseExpression expr
    case result of
        FromResponseBody path -> path === expr
        _other -> assert False

propParseRequestBody :: Property
propParseRequestBody = property $ do
    let expr = "$request.body#/user/name"
        result = parseExpression expr
    case result of
        FromState key -> key === "requestBody/user/name"
        _other -> assert False

-- parseRuntimeExpression properties

propRuntimeResponseBody :: Property
propRuntimeResponseBody = property $ do
    let expr = "$response.body#/user/id"
        result = parseRuntimeExpression expr
    case result of
        Right (source, mPointer) -> do
            source === "response.body"
            mPointer === Just "/user/id"
        Left _ -> assert False

propRuntimeResponseHeader :: Property
propRuntimeResponseHeader = property $ do
    header <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$response.header." <> header
        result = parseRuntimeExpression expr
    case result of
        Right (source, mPointer) -> do
            source === ("response.header." <> header)
            mPointer === Nothing
        Left _ -> assert False

propRuntimeNonExpression :: Property
propRuntimeNonExpression = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
    let result = parseRuntimeExpression txt
    case result of
        Left err -> assert $ T.isInfixOf "Not a runtime expression" err
        Right _ -> assert False

propRuntimeUnsupported :: Property
propRuntimeUnsupported = property $ do
    let result = parseRuntimeExpression "$method"
    case result of
        Left err -> assert $ T.isInfixOf "Unsupported" err
        Right _ -> assert False

propRuntimeRequestPath :: Property
propRuntimeRequestPath = property $ do
    param <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let expr = "$request.path." <> param
        result = parseRuntimeExpression expr
    case result of
        Right (source, mPointer) -> do
            source === ("request.path." <> param)
            mPointer === Nothing
        Left _ -> assert False

propRuntimeUrl :: Property
propRuntimeUrl = property $ do
    let result = parseRuntimeExpression "$url"
    case result of
        Right (source, mPointer) -> do
            source === "url"
            mPointer === Nothing
        Left _ -> assert False

-- decodeJsonPointer properties

propDecodeSlash :: Property
propDecodeSlash = property $ do
    prefix <- forAll $ Gen.text (Range.linear 0 10) Gen.alpha
    suffix <- forAll $ Gen.text (Range.linear 0 10) Gen.alpha
    let encoded = prefix <> "~1" <> suffix
        decoded = decodeJsonPointer encoded
    decoded === (prefix <> "/" <> suffix)

propDecodeTilde :: Property
propDecodeTilde = property $ do
    prefix <- forAll $ Gen.text (Range.linear 0 10) Gen.alpha
    suffix <- forAll $ Gen.text (Range.linear 0 10) Gen.alpha
    let encoded = prefix <> "~0" <> suffix
        decoded = decodeJsonPointer encoded
    decoded === (prefix <> "~" <> suffix)

propDecodeMixed :: Property
propDecodeMixed = property $ do
    let encoded = "a~1b~0c~1d"
        decoded = decodeJsonPointer encoded
    decoded === "a/b~c/d"

propDecodePreserve :: Property
propDecodePreserve = property $ do
    txt <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
    let decoded = decodeJsonPointer txt
    decoded === txt

propDecodeEmpty :: Property
propDecodeEmpty = property $ do
    let decoded = decodeJsonPointer ""
    decoded === ""

-- parseOperationRef properties

propParseOpRefValid :: Property
propParseOpRefValid = property $ do
    let ref = Just "#/paths/~1users/get"
        result = parseOperationRef ref
    result === Just "GET /users"

propParseOpRefInvalid :: Property
propParseOpRefInvalid = property $ do
    let ref = Just "#/components/schemas/User"
        result = parseOperationRef ref
    result === Nothing

propParseOpRefNothing :: Property
propParseOpRefNothing = property $ do
    let result = parseOperationRef Nothing
    result === Nothing

propParseOpRefEncoded :: Property
propParseOpRefEncoded = property $ do
    -- ~1 encodes /, so ~1users~1{userId} -> /users/{userId}
    let ref = Just "#/paths/~1users~1{userId}/delete"
        result = parseOperationRef ref
    result === Just "DELETE /users/{userId}"
