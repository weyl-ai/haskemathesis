{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.ResponseHeaders (spec) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Hedgehog (Property, failure, property, success)
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Standard (responseHeadersConformance)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResponseSpec (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Response headers" $ do
        itProp "integer header" prop_response_header_schema_integer
        itProp "array header" prop_response_header_schema_array
        itProp "multiple header values" prop_response_header_schema_multiple_values
        itProp "quoted string header" prop_response_header_schema_quoted_string
        itProp "case-insensitive header names" prop_response_header_schema_case_insensitive
        itProp "whitespace trimmed" prop_response_header_schema_whitespace_trim
        itProp "array of strings" prop_response_header_schema_array_strings
        itProp "array integer invalid fails" prop_response_header_schema_array_integer_invalid_fails
        itProp "integer invalid fails" prop_response_header_schema_integer_invalid_fails
        itProp "required headers missing fails" prop_response_headers_required_missing_fails

prop_response_header_schema_integer :: Property
prop_response_header_schema_integer =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = Map.fromList [("X-Rate", emptySchema{schemaType = Just SInteger})]
                    , rsRequiredHeaders = ["X-Rate"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Rate", "42")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_array :: Property
prop_response_header_schema_array =
    property $ do
        let itemSchema = emptySchema{schemaType = Just SInteger}
            responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders =
                        Map.fromList
                            [
                                ( "X-Ids"
                                , emptySchema
                                    { schemaType = Just SArray
                                    , schemaItems = Just itemSchema
                                    }
                                )
                            ]
                    , rsRequiredHeaders = ["X-Ids"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Ids", "1,2,3")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_multiple_values :: Property
prop_response_header_schema_multiple_values =
    property $ do
        let itemSchema = emptySchema{schemaType = Just SInteger}
            responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders =
                        Map.fromList
                            [
                                ( "X-Ids"
                                , emptySchema
                                    { schemaType = Just SArray
                                    , schemaItems = Just itemSchema
                                    }
                                )
                            ]
                    , rsRequiredHeaders = ["X-Ids"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Ids", "1"), (CI.mk "X-Ids", "2")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_quoted_string :: Property
prop_response_header_schema_quoted_string =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = Map.fromList [("X-Tag", emptySchema{schemaType = Just SString})]
                    , rsRequiredHeaders = ["X-Tag"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Tag", "\"hello\"")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_case_insensitive :: Property
prop_response_header_schema_case_insensitive =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = Map.fromList [("X-Rate", emptySchema{schemaType = Just SInteger})]
                    , rsRequiredHeaders = ["x-rate"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "x-rate", "7")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_whitespace_trim :: Property
prop_response_header_schema_whitespace_trim =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = Map.fromList [("X-Tag", emptySchema{schemaType = Just SString})]
                    , rsRequiredHeaders = ["X-Tag"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Tag", "  hello  ")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_array_strings :: Property
prop_response_header_schema_array_strings =
    property $ do
        let itemSchema = emptySchema{schemaType = Just SString}
            responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders =
                        Map.fromList
                            [
                                ( "X-Names"
                                , emptySchema
                                    { schemaType = Just SArray
                                    , schemaItems = Just itemSchema
                                    }
                                )
                            ]
                    , rsRequiredHeaders = ["X-Names"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Names", "alice,bob")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_header_schema_array_integer_invalid_fails :: Property
prop_response_header_schema_array_integer_invalid_fails =
    property $ do
        let itemSchema = emptySchema{schemaType = Just SInteger}
            responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders =
                        Map.fromList
                            [
                                ( "X-Ids"
                                , emptySchema
                                    { schemaType = Just SArray
                                    , schemaItems = Just itemSchema
                                    }
                                )
                            ]
                    , rsRequiredHeaders = ["X-Ids"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Ids", "1,foo")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_header_schema_integer_invalid_fails :: Property
prop_response_header_schema_integer_invalid_fails =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = Map.fromList [("X-Rate", emptySchema{schemaType = Just SInteger})]
                    , rsRequiredHeaders = ["X-Rate"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(CI.mk "X-Rate", "oops")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_headers_required_missing_fails :: Property
prop_response_headers_required_missing_fails =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = mempty
                    , rsRequiredHeaders = ["X-Rate"]
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseHeadersConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure
