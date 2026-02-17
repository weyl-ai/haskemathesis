{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Negative (spec) where

import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hedgehog (Property, assert, discard, forAll, property, success)
import Test.Hspec (Spec, describe)

import qualified Data.CaseInsensitive as CI

import Haskemathesis.Check.Negative (negativeTestRejection)
import Haskemathesis.Check.Types (CheckResult (..), FailureDetail (..))
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.Gen.Negative (NegativeMutation (..), applyNegativeMutation, genNegativeRequest, renderNegativeMutation)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ParamLocation (..), ResolvedOperation (..), ResolvedParam (..), ResolvedRequestBody (..))
import Haskemathesis.Property (propertiesForSpecWithConfig)
import Haskemathesis.Schema
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)
import Haskemathesis.Validate (validateErrors)

spec :: Spec
spec =
    describe "Negative testing" $ do
        itProp "negative request generator produces mutation when possible" prop_negative_request_generated
        itProp "negative request generator returns Nothing when no candidates" prop_negative_request_none_when_no_candidates
        itProp "remove required query preserves other params" prop_negative_remove_required_query_preserves_others
        itProp "remove required header preserves other headers" prop_negative_remove_required_header_preserves_others
        itProp "remove required path param yields mutation" prop_negative_remove_required_path_param
        itProp "invalid integer path param yields non-numeric" prop_negative_invalid_integer_path_param
        itProp "invalid integer header uses non-numeric" prop_negative_invalid_integer_header
        itProp "invalid boolean header uses non-boolean" prop_negative_invalid_boolean_header
        itProp "invalid integer query uses non-numeric" prop_negative_invalid_integer_query
        itProp "invalid boolean query uses non-boolean" prop_negative_invalid_boolean_query
        itProp "invalid enum query not in enum" prop_negative_invalid_enum_query
        itProp "invalid enum header not in enum" prop_negative_invalid_enum_header
        itProp "invalid body fails schema validation" prop_negative_invalid_body_fails_schema
        itProp "invalid content-type uses text/plain" prop_negative_invalid_content_type_is_text_plain
        itProp "invalid content-type preserves payload" prop_negative_invalid_content_type_preserves_payload
        itProp "invalid body for string schema is non-string" prop_negative_invalid_body_string_non_string
        itProp "mutation label is non-empty" prop_negative_mutation_label_non_empty
        itProp "negative properties appear when enabled" prop_negative_properties_enabled
        itProp "negative properties not present when disabled" prop_negative_properties_disabled
        itProp "negative test rejection passes for 4xx" prop_negative_rejection_passes_4xx
        itProp "negative test rejection expects 4xx" prop_negative_rejection_requires_4xx
        itProp "negative test rejection fails for 2xx" prop_negative_rejection_fails_2xx
        itProp "negative failure includes mutation" prop_negative_failure_includes_mutation

prop_negative_request_generated :: Property
prop_negative_request_generated =
    property $ do
        let op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = emptySchema{schemaType = Just SString}
                                }
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Nothing -> assert False
            Just _ -> success

prop_negative_request_none_when_no_candidates :: Property
prop_negative_request_none_when_no_candidates =
    property $ do
        let op = emptyOperation
        mReq <- forAll (genNegativeRequest op)
        assert (isNothing mReq)

prop_negative_remove_required_query_preserves_others :: Property
prop_negative_remove_required_query_preserves_others =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "a"
                            , rpLocation = ParamQuery
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SString, schemaConst = Just (String "a")}
                            }
                        , ResolvedParam
                            { rpName = "b"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SString, schemaConst = Just (String "b")}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, RemoveRequiredQuery "a") -> do
                assert (("a", "a") `notElem` reqQueryParams req)
                assert (("b", "b") `elem` reqQueryParams req)
            Just _ -> discard
            Nothing -> assert False

prop_negative_remove_required_header_preserves_others :: Property
prop_negative_remove_required_header_preserves_others =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "X-A"
                            , rpLocation = ParamHeader
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SString}
                            }
                        , ResolvedParam
                            { rpName = "X-B"
                            , rpLocation = ParamHeader
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SString}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, RemoveRequiredHeader "X-A") -> do
                assert (not (any ((== CI.mk (encodeUtf8 "X-A")) . fst) (reqHeaders req)))
                assert (any ((== CI.mk (encodeUtf8 "X-B")) . fst) (reqHeaders req))
            Just _ -> discard
            Nothing -> assert False

prop_negative_remove_required_path_param :: Property
prop_negative_remove_required_path_param =
    property $ do
        let op =
                emptyOperation
                    { roPath = "/items/{id}"
                    , roParameters =
                        [ ResolvedParam
                            { rpName = "id"
                            , rpLocation = ParamPath
                            , rpRequired = True
                            , rpSchema = emptySchema{schemaType = Just SString}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, RemoveRequiredPath "id") -> assert ("/items/" `T.isPrefixOf` reqPath req)
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_integer_path_param :: Property
prop_negative_invalid_integer_path_param =
    property $ do
        let op =
                emptyOperation
                    { roPath = "/items/{id}"
                    , roParameters =
                        [ ResolvedParam
                            { rpName = "id"
                            , rpLocation = ParamPath
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidPathParam "id") -> assert ("not-a-number" `T.isInfixOf` reqPath req)
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_integer_header :: Property
prop_negative_invalid_integer_header =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "X-Int"
                            , rpLocation = ParamHeader
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidHeader "X-Int") ->
                assert (any ((== encodeUtf8 "not-a-number") . snd) (reqHeaders req))
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_boolean_header :: Property
prop_negative_invalid_boolean_header =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "X-Flag"
                            , rpLocation = ParamHeader
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SBoolean}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidHeader "X-Flag") ->
                assert (any ((== encodeUtf8 "not-a-boolean") . snd) (reqHeaders req))
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_integer_query :: Property
prop_negative_invalid_integer_query =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "count"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidQueryParam "count") -> assert (("count", "not-a-number") `elem` reqQueryParams req)
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_boolean_query :: Property
prop_negative_invalid_boolean_query =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "flag"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SBoolean}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidQueryParam "flag") -> assert (("flag", "not-a-boolean") `elem` reqQueryParams req)
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_enum_query :: Property
prop_negative_invalid_enum_query =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "color"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaEnum = Just [String "red", String "blue"]}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidQueryParam "color") -> do
                let value = lookup "color" (reqQueryParams req)
                assert (value == Just "not-in-enum" || value == Just "not-in-enum-1")
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_enum_header :: Property
prop_negative_invalid_enum_header =
    property $ do
        let op =
                emptyOperation
                    { roParameters =
                        [ ResolvedParam
                            { rpName = "X-Mode"
                            , rpLocation = ParamHeader
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaEnum = Just [String "fast", String "slow"]}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidHeader "X-Mode") -> do
                let value = lookup (CI.mk (encodeUtf8 "X-Mode")) (reqHeaders req)
                let rendered = decodeUtf8 <$> value
                assert (rendered == Just "not-in-enum" || rendered == Just "not-in-enum-1")
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_body_fails_schema :: Property
prop_negative_invalid_body_fails_schema =
    property $ do
        let schema = emptySchema{schemaType = Just SInteger}
            op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = schema
                                }
                    }
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidRequestBody
        case reqBody mutated of
            Just (_ct, body) ->
                let value = decodeBody (LBS.fromStrict body)
                 in assert (not (null (validateErrors schema value)))
            Nothing -> assert False

prop_negative_invalid_content_type_is_text_plain :: Property
prop_negative_invalid_content_type_is_text_plain =
    property $ do
        let schema = emptySchema{schemaType = Just SString}
            op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = schema
                                }
                    }
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidContentType
        case reqBody mutated of
            Just (ct, _body) -> assert (ct == "text/plain")
            Nothing -> assert False

prop_negative_invalid_content_type_preserves_payload :: Property
prop_negative_invalid_content_type_preserves_payload =
    property $ do
        let schema = emptySchema{schemaType = Just SString}
            op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = schema
                                }
                    }
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidContentType
        case reqBody mutated of
            Just (_ct, body) -> assert (body /= "")
            Nothing -> assert False

prop_negative_invalid_body_string_non_string :: Property
prop_negative_invalid_body_string_non_string =
    property $ do
        let schema = emptySchema{schemaType = Just SString}
            op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = schema
                                }
                    }
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidRequestBody
        case reqBody mutated of
            Just (_ct, body) ->
                let value = decodeBody (LBS.fromStrict body)
                 in assert (case value of String _ -> False; _ -> True)
            Nothing -> assert False

prop_negative_mutation_label_non_empty :: Property
prop_negative_mutation_label_non_empty =
    property $ do
        let op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = emptySchema{schemaType = Just SString}
                                }
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (_req, mutation) -> assert (not (T.null (renderNegativeMutation mutation)))
            Nothing -> assert False

prop_negative_rejection_requires_4xx :: Property
prop_negative_rejection_requires_4xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 500
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "oops"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed _ -> success
            CheckPassed -> assert False

prop_negative_rejection_passes_4xx :: Property
prop_negative_rejection_passes_4xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 400
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "oops"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckPassed -> success
            CheckFailed _ -> assert False

prop_negative_rejection_fails_2xx :: Property
prop_negative_rejection_fails_2xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "ok"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed _ -> success
            CheckPassed -> assert False

prop_negative_failure_includes_mutation :: Property
prop_negative_failure_includes_mutation =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed detail -> assert (fdMutation detail == Just "invalid body")
            CheckPassed -> assert False

prop_negative_properties_enabled :: Property
prop_negative_properties_enabled =
    property $ do
        let op = emptyOperation
            config = defaultTestConfig{tcNegativeTesting = True}
            props = propertiesForSpecWithConfig mempty config (const (pure (ApiResponse 400 [] "" 0))) [op]
        assert (any (T.isPrefixOf "NEGATIVE:" . fst) props)

prop_negative_properties_disabled :: Property
prop_negative_properties_disabled =
    property $ do
        let op = emptyOperation
            config = defaultTestConfig{tcNegativeTesting = False}
            props = propertiesForSpecWithConfig mempty config (const (pure (ApiResponse 400 [] "" 0))) [op]
        assert (not (any (T.isPrefixOf "NEGATIVE:" . fst) props))

decodeBody :: LBS.ByteString -> Value
decodeBody body =
    case eitherDecode body of
        Right value -> value
        Left _ -> String "invalid-json"
