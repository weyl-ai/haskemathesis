{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.ResponseSchema (spec) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Hedgehog (Property, assert, failure, property, success)
import Test.Hspec (Spec, describe)

import Network.HTTP.Types (hContentType)

import Haskemathesis.Check.Standard (responseSchemaConformance)
import Haskemathesis.Check.Types (Check (..), CheckResult (..), FailureDetail (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResponseSpec (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Response schema conformance" $ do
        itProp "schema errors reported" prop_schema_errors_reported
        itProp "without content-type and single schema" prop_response_schema_without_content_type_single_schema
        itProp "without content-type and multiple schemas" prop_response_schema_without_content_type_multiple_schemas_skips
        itProp "invalid json fails" prop_response_schema_conformance_invalid_json_fails
        itProp "null allowed when nullable" prop_response_schema_conformance_null_allows_nullable
        itProp "empty body fails" prop_response_schema_conformance_empty_body_fails
        itProp "no schema skips" prop_response_schema_conformance_no_schema_skips
        itProp "default response used" prop_response_schema_conformance_default_used
        itProp "integer schema ok" prop_response_schema_conformance_integer_ok
        itProp "number schema ok" prop_response_schema_conformance_number_ok

prop_schema_errors_reported :: Property
prop_schema_errors_reported =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = LBS.toStrict (encode ("oops" :: Text))
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed detail -> assert (not (null (fdSchemaErrors detail)))
            _otherResult -> failure

prop_response_schema_without_content_type_single_schema :: Property
prop_response_schema_without_content_type_single_schema =
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
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_without_content_type_multiple_schemas_skips :: Property
prop_response_schema_without_content_type_multiple_schemas_skips =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent =
                        Map.fromList
                            [ ("application/json", emptySchema{schemaType = Just SString})
                            , ("text/plain", emptySchema{schemaType = Just SString})
                            ]
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
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_invalid_json_fails :: Property
prop_response_schema_conformance_invalid_json_fails =
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
                    , resBody = "not-json"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_schema_conformance_null_allows_nullable :: Property
prop_response_schema_conformance_null_allows_nullable =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent =
                        Map.fromList
                            [
                                ( "application/json"
                                , emptySchema
                                    { schemaType = Just SString
                                    , schemaNullable = True
                                    }
                                )
                            ]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "null"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_empty_body_fails :: Property
prop_response_schema_conformance_empty_body_fails =
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
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_schema_conformance_no_schema_skips :: Property
prop_response_schema_conformance_no_schema_skips =
    property $ do
        let op = emptyOperation
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "not-json"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_default_used :: Property
prop_response_schema_conformance_default_used =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roDefaultResponse = Just responseSpec}
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_integer_ok :: Property
prop_response_schema_conformance_integer_ok =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_number_ok :: Property
prop_response_schema_conformance_number_ok =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SNumber})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1.5"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure
