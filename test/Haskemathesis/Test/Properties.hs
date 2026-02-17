{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties (
    tests,
) where

import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.HTTP.Types (hContentType)

import Haskemathesis.Check.Standard
import Haskemathesis.Check.Types
import Haskemathesis.Execute.Types
import Haskemathesis.Gen
import Haskemathesis.Gen.Request
import Haskemathesis.OpenApi.Types
import Haskemathesis.Report.Curl
import Haskemathesis.Report.Render
import Haskemathesis.Schema
import Haskemathesis.Test.Generators (genSchema)
import Haskemathesis.Validate

tests :: Group
tests =
    Group
        "haskemathesis"
        [ ("prop_generated_values_validate", prop_generated_values_validate)
        , ("prop_string_length", prop_string_length)
        , ("prop_integer_bounds", prop_integer_bounds)
        , ("prop_number_bounds", prop_number_bounds)
        , ("prop_array_bounds", prop_array_bounds)
        , ("prop_object_required", prop_object_required)
        , ("prop_nullable_allows_null", prop_nullable_allows_null)
        , ("prop_anyof_oneof_allof", prop_anyof_oneof_allof)
        , ("prop_request_path_interpolation", prop_request_path_interpolation)
        , ("prop_request_header_generation", prop_request_header_generation)
        , ("prop_content_type_suffix_json", prop_content_type_suffix_json)
        , ("prop_response_header_schema_integer", prop_response_header_schema_integer)
        , ("prop_response_header_schema_array", prop_response_header_schema_array)
        , ("prop_schema_errors_reported", prop_schema_errors_reported)
        , ("prop_request_body_sets_media_type", prop_request_body_sets_media_type)
        , ("prop_curl_includes_content_type_header_when_body", prop_curl_includes_content_type_header_when_body)
        , ("prop_curl_respects_existing_content_type_header", prop_curl_respects_existing_content_type_header)
        , ("prop_response_header_schema_multiple_values", prop_response_header_schema_multiple_values)
        , ("prop_response_header_schema_quoted_string", prop_response_header_schema_quoted_string)
        , ("prop_status_code_conformance_default_allows", prop_status_code_conformance_default_allows)
        , ("prop_status_code_conformance_missing_fails", prop_status_code_conformance_missing_fails)
        , ("prop_response_schema_without_content_type_single_schema", prop_response_schema_without_content_type_single_schema)
        , ("prop_response_schema_without_content_type_multiple_schemas_skips", prop_response_schema_without_content_type_multiple_schemas_skips)
        , ("prop_content_type_conformance_requires_header", prop_content_type_conformance_requires_header)
        , ("prop_content_type_conformance_exact_match", prop_content_type_conformance_exact_match)
        , ("prop_response_header_schema_integer_invalid_fails", prop_response_header_schema_integer_invalid_fails)
        , ("prop_content_type_conformance_mismatch_fails", prop_content_type_conformance_mismatch_fails)
        , ("prop_response_schema_conformance_invalid_json_fails", prop_response_schema_conformance_invalid_json_fails)
        , ("prop_status_code_conformance_explicit_allows", prop_status_code_conformance_explicit_allows)
        , ("prop_response_schema_conformance_null_allows_nullable", prop_response_schema_conformance_null_allows_nullable)
        , ("prop_response_schema_conformance_empty_body_fails", prop_response_schema_conformance_empty_body_fails)
        , ("prop_request_query_params_rendered_in_curl", prop_request_query_params_rendered_in_curl)
        , ("prop_response_header_schema_case_insensitive", prop_response_header_schema_case_insensitive)
        , ("prop_response_header_schema_whitespace_trim", prop_response_header_schema_whitespace_trim)
        , ("prop_content_type_conformance_with_params", prop_content_type_conformance_with_params)
        , ("prop_request_query_params_duplicate_in_curl", prop_request_query_params_duplicate_in_curl)
        , ("prop_response_header_schema_array_strings", prop_response_header_schema_array_strings)
        , ("prop_curl_includes_base_url", prop_curl_includes_base_url)
        , ("prop_curl_omits_body_when_none", prop_curl_omits_body_when_none)
        , ("prop_curl_encodes_spaces_in_query", prop_curl_encodes_spaces_in_query)
        , ("prop_content_type_suffix_json_with_params", prop_content_type_suffix_json_with_params)
        , ("prop_response_schema_conformance_no_schema_skips", prop_response_schema_conformance_no_schema_skips)
        , ("prop_response_schema_conformance_default_used", prop_response_schema_conformance_default_used)
        , ("prop_response_headers_required_missing_fails", prop_response_headers_required_missing_fails)
        , ("prop_response_header_schema_array_integer_invalid_fails", prop_response_header_schema_array_integer_invalid_fails)
        , ("prop_request_path_multiple_params", prop_request_path_multiple_params)
        , ("prop_request_header_value_with_spaces_in_curl", prop_request_header_value_with_spaces_in_curl)
        , ("prop_render_failure_includes_schema_errors", prop_render_failure_includes_schema_errors)
        , ("prop_render_failure_includes_seed", prop_render_failure_includes_seed)
        , ("prop_validate_unique_items_violation", prop_validate_unique_items_violation)
        , ("prop_validate_min_items_violation", prop_validate_min_items_violation)
        , ("prop_validate_max_items_violation", prop_validate_max_items_violation)
        , ("prop_validate_string_length_violation", prop_validate_string_length_violation)
        , ("prop_curl_encodes_reserved_chars_in_query", prop_curl_encodes_reserved_chars_in_query)
        , ("prop_response_schema_conformance_integer_ok", prop_response_schema_conformance_integer_ok)
        , ("prop_response_schema_conformance_number_ok", prop_response_schema_conformance_number_ok)
        , ("prop_render_failure_includes_request_and_response", prop_render_failure_includes_request_and_response)
        ]

prop_generated_values_validate :: Property
prop_generated_values_validate =
    property $ do
        schema <- forAll (genSchema 2)
        value <- forAll (genFromSchema schema)
        assert (validateValue schema value)

prop_string_length :: Property
prop_string_length =
    property $ do
        minL <- forAll (Gen.int (Range.linear 0 5))
        maxL <- forAll (Gen.int (Range.linear minL (minL + 10)))
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaMinLength = Just minL
                    , schemaMaxLength = Just maxL
                    }
        value <- forAll (genFromSchema schema)
        case value of
            String txt -> do
                let len = BS.length (encodeUtf8 txt)
                assert (len >= minL)
                assert (len <= maxL)
            _otherValue -> failure

prop_integer_bounds :: Property
prop_integer_bounds =
    property $ do
        lo <- forAll (Gen.int (Range.linear (-50) 50))
        useExclusive <- forAll Gen.bool
        hi <-
            if useExclusive
                then forAll (Gen.int (Range.linear (lo + 2) (lo + 100)))
                else forAll (Gen.int (Range.linear lo (lo + 100)))
        let schema =
                emptySchema
                    { schemaType = Just SInteger
                    , schemaMinimum = Just (fromIntegral lo)
                    , schemaMaximum = Just (fromIntegral hi)
                    , schemaExclusiveMinimum = if useExclusive then Just (fromIntegral lo) else Nothing
                    , schemaExclusiveMaximum = if useExclusive then Just (fromIntegral hi) else Nothing
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Number n -> do
                let d = realToFrac n :: Double
                if useExclusive
                    then do
                        assert (d > fromIntegral lo)
                        assert (d < fromIntegral hi)
                    else do
                        assert (d >= fromIntegral lo)
                        assert (d <= fromIntegral hi)
            _otherValue -> failure

prop_number_bounds :: Property
prop_number_bounds =
    property $ do
        lo <- forAll (Gen.double (Range.linearFrac (-50) 50))
        useExclusive <- forAll Gen.bool
        hi <-
            if useExclusive
                then forAll (Gen.double (Range.linearFrac (lo + 1) (lo + 100)))
                else forAll (Gen.double (Range.linearFrac lo (lo + 100)))
        let schema =
                emptySchema
                    { schemaType = Just SNumber
                    , schemaMinimum = Just lo
                    , schemaMaximum = Just hi
                    , schemaExclusiveMinimum = if useExclusive then Just lo else Nothing
                    , schemaExclusiveMaximum = if useExclusive then Just hi else Nothing
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Number n -> do
                let d = realToFrac n :: Double
                if useExclusive
                    then do
                        assert (d > lo)
                        assert (d < hi)
                    else do
                        assert (d >= lo)
                        assert (d <= hi)
            _otherValue -> failure

prop_array_bounds :: Property
prop_array_bounds =
    property $ do
        minI <- forAll (Gen.int (Range.linear 0 3))
        maxI <- forAll (Gen.int (Range.linear minI (minI + 6)))
        unique <- forAll Gen.bool
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMinItems = Just minI
                    , schemaMaxItems = Just maxI
                    , schemaUniqueItems = unique
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Array vec -> do
                let len = Vector.length vec
                assert (len >= minI)
                assert (len <= maxI)
                if unique
                    then assert (validateValue schema value)
                    else success
            _otherValue -> failure

prop_object_required :: Property
prop_object_required =
    property $ do
        let props =
                Map.fromList
                    [ ("a", emptySchema{schemaType = Just SString})
                    , ("b", emptySchema{schemaType = Just SInteger})
                    ]
            schema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = props
                    , schemaRequired = ["a"]
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Object obj ->
                assert (KeyMap.member (Key.fromText "a") obj)
            _otherValue -> failure

prop_nullable_allows_null :: Property
prop_nullable_allows_null =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaNullable = True
                    }
        value <- forAll (genFromSchema schema)
        assert (validateValue schema value)

prop_anyof_oneof_allof :: Property
prop_anyof_oneof_allof =
    property $ do
        let s1 = emptySchema{schemaType = Just SString}
            s2 = emptySchema{schemaType = Just SInteger}
            anySchema = emptySchema{schemaAnyOf = [s1, s2]}
            oneSchema = emptySchema{schemaOneOf = [s1, s2]}
            allSchema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("a", emptySchema{schemaType = Just SString})]
                    , schemaRequired = ["a"]
                    , schemaAllOf = [emptySchema{schemaType = Just SObject}]
                    }
        vAny <- forAll (genFromSchema anySchema)
        vOne <- forAll (genFromSchema oneSchema)
        vAll <- forAll (genFromSchema allSchema)
        assert (validateValue anySchema vAny)
        assert (validateValue oneSchema vOne)
        assert (validateValue allSchema vAll)

prop_request_path_interpolation :: Property
prop_request_path_interpolation =
    property $ do
        let op =
                emptyOperation
                    { roPath = "/items/{id}"
                    , roParameters =
                        [ ResolvedParam
                            { rpName = "id"
                            , rpLocation = ParamPath
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        ]
                    }
        req <- forAll (genApiRequest op)
        assert (not ("{id}" `T.isInfixOf` reqPath req))

prop_request_header_generation :: Property
prop_request_header_generation =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "X-Test"
                            , rpLocation = ParamHeader
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SString}
                            }
                        ]
                    }
        req <- forAll (genApiRequest op)
        let headerNames = map fst (reqHeaders req)
        assert (CI.mk (encodeUtf8 "X-Test") `elem` headerNames)

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

prop_request_body_sets_media_type :: Property
prop_request_body_sets_media_type =
    property $ do
        let bodySchema = emptySchema{schemaType = Just SString}
            op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = bodySchema
                                }
                    }
        req <- forAll (genApiRequest op)
        case reqBody req of
            Just (mediaType, _body) -> mediaType === "application/json"
            Nothing -> failure

prop_curl_includes_content_type_header_when_body :: Property
prop_curl_includes_content_type_header_when_body =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "POST"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Just ("application/json", encodeUtf8 "{}")
                    }
            curl = toCurl Nothing req
        assert ("-H 'Content-Type: application/json'" `T.isInfixOf` curl)
        assert ("-d '" `T.isInfixOf` curl)

prop_curl_respects_existing_content_type_header :: Property
prop_curl_respects_existing_content_type_header =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "POST"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = [(CI.mk "Content-Type", "application/json")]
                    , reqBody = Just ("application/json", encodeUtf8 "{}")
                    }
            curl = toCurl Nothing req
        T.count "Content-Type: application/json" curl === 1

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

prop_status_code_conformance_default_allows :: Property
prop_status_code_conformance_default_allows =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roDefaultResponse = Just responseSpec}
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_status_code_conformance_missing_fails :: Property
prop_status_code_conformance_missing_fails =
    property $ do
        let op = emptyOperation
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

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

prop_status_code_conformance_explicit_allows :: Property
prop_status_code_conformance_explicit_allows =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(201, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 201
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

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

prop_request_query_params_rendered_in_curl :: Property
prop_request_query_params_rendered_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "hello"), ("page", "1")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/search?q=hello&page=1" `T.isInfixOf` curl)

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

prop_request_query_params_duplicate_in_curl :: Property
prop_request_query_params_duplicate_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = [("tag", "a"), ("tag", "b")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/items?tag=a&tag=b" `T.isInfixOf` curl)

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

prop_curl_includes_base_url :: Property
prop_curl_includes_base_url =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl (Just "https://example.com") req
        assert ("https://example.com/items" `T.isInfixOf` curl)

prop_curl_omits_body_when_none :: Property
prop_curl_omits_body_when_none =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert (not ("-d " `T.isInfixOf` curl))

prop_curl_encodes_spaces_in_query :: Property
prop_curl_encodes_spaces_in_query =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "hello world")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert
            ( "/search?q=hello+world" `T.isInfixOf` curl
                || "/search?q=hello%20world" `T.isInfixOf` curl
            )

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

prop_request_path_multiple_params :: Property
prop_request_path_multiple_params =
    property $ do
        let op =
                emptyOperation
                    { roPath = "/items/{id}/tags/{tag}"
                    , roParameters =
                        [ ResolvedParam
                            { rpName = "id"
                            , rpLocation = ParamPath
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        , ResolvedParam
                            { rpName = "tag"
                            , rpLocation = ParamPath
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SString}
                            }
                        ]
                    }
        req <- forAll (genApiRequest op)
        assert (not ("{id}" `T.isInfixOf` reqPath req))
        assert (not ("{tag}" `T.isInfixOf` reqPath req))

prop_request_header_value_with_spaces_in_curl :: Property
prop_request_header_value_with_spaces_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = [(CI.mk "X-Note", "hello world")]
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("X-Note: hello world" `T.isInfixOf` curl)

prop_render_failure_includes_schema_errors :: Property
prop_render_failure_includes_schema_errors =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = ["err-1"]
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Schema errors: err-1" `T.isInfixOf` rendered)

prop_render_failure_includes_seed :: Property
prop_render_failure_includes_seed =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = []
                    }
            rendered = renderFailureDetail Nothing (Just "seed-123") detail
        assert ("Seed: seed-123" `T.isInfixOf` rendered)

prop_validate_unique_items_violation :: Property
prop_validate_unique_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaUniqueItems = True
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1, Number 1])
        assert (not (validateValue schema value))

prop_validate_min_items_violation :: Property
prop_validate_min_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMinItems = Just 2
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1])
        assert (not (validateValue schema value))

prop_validate_max_items_violation :: Property
prop_validate_max_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMaxItems = Just 1
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1, Number 2])
        assert (not (validateValue schema value))

prop_validate_string_length_violation :: Property
prop_validate_string_length_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaMinLength = Just 2
                    }
            value = String "a"
        assert (not (validateValue schema value))

prop_curl_encodes_reserved_chars_in_query :: Property
prop_curl_encodes_reserved_chars_in_query =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "a&b=c")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/search?q=a%26b%3Dc" `T.isInfixOf` curl)

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

prop_render_failure_includes_request_and_response :: Property
prop_render_failure_includes_request_and_response =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = [("q", "a")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            res =
                ApiResponse
                    { resStatusCode = 500
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
            detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = req
                    , fdResponse = res
                    , fdOperation = "GET /items"
                    , fdSchemaErrors = []
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Request: GET /items?q=a" `T.isInfixOf` rendered)
        assert ("Response: status=500" `T.isInfixOf` rendered)

emptyOperation :: ResolvedOperation
emptyOperation =
    ResolvedOperation
        { roMethod = "GET"
        , roPath = "/"
        , roOperationId = Nothing
        , roParameters = []
        , roRequestBody = Nothing
        , roResponses = mempty
        , roDefaultResponse = Nothing
        }

dummyRequest :: ResolvedOperation -> ApiRequest
dummyRequest op =
    ApiRequest
        { reqMethod = encodeUtf8 (roMethod op)
        , reqPath = roPath op
        , reqQueryParams = []
        , reqHeaders = []
        , reqBody = Nothing
        }
