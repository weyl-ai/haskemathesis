{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.ContentType (spec) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Hedgehog (Property, failure, property, success)
import Test.Hspec (Spec, describe)

import Network.HTTP.Types (hContentType)

import Haskemathesis.Check.Standard (contentTypeConformance)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResponseSpec (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Content type conformance" $ do
        itProp "suffix json" prop_content_type_suffix_json
        itProp "suffix json with params" prop_content_type_suffix_json_with_params
        itProp "requires header" prop_content_type_conformance_requires_header
        itProp "exact match" prop_content_type_conformance_exact_match
        itProp "mismatch fails" prop_content_type_conformance_mismatch_fails
        itProp "matches with params" prop_content_type_conformance_with_params

prop_content_type_suffix_json :: Property
prop_content_type_suffix_json =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/vnd.api+json")]
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_content_type_suffix_json_with_params :: Property
prop_content_type_suffix_json_with_params =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/vnd.api+json; charset=utf-8")]
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_content_type_conformance_requires_header :: Property
prop_content_type_conformance_requires_header =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_content_type_conformance_exact_match :: Property
prop_content_type_conformance_exact_match =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_content_type_conformance_mismatch_fails :: Property
prop_content_type_conformance_mismatch_fails =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "text/plain")]
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_content_type_conformance_with_params :: Property
prop_content_type_conformance_with_params =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json; charset=utf-8")]
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun contentTypeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure
