{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Negative.Mutations (spec) where

import Data.Aeson (Value (..), eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, assert, discard, forAll, property, success)
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.Gen.Negative (NegativeMutation (..), applyNegativeMutation, genNegativeRequest, renderNegativeMutation)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ParamLocation (..), ResolvedOperation (..), ResolvedParam (..), ResolvedRequestBody (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "Negative mutations" $ do
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
                            { rpName = "X-Bool"
                            , rpLocation = ParamHeader
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SBoolean}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidHeader "X-Bool") ->
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
                            { rpName = "q"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaType = Just SInteger}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidQueryParam "q") -> assert (("q", "not-a-number") `elem` reqQueryParams req)
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
                            { rpName = "mode"
                            , rpLocation = ParamQuery
                            , rpRequired = False
                            , rpSchema = emptySchema{schemaEnum = Just [String "fast", String "slow"]}
                            }
                        ]
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Just (req, InvalidQueryParam "mode") -> assert (("mode", "not-in-enum") `elem` reqQueryParams req)
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
            Just (req, InvalidHeader "X-Mode") ->
                assert (any ((== encodeUtf8 "not-in-enum") . snd) (reqHeaders req))
            Just _ -> discard
            Nothing -> assert False

prop_negative_invalid_body_fails_schema :: Property
prop_negative_invalid_body_fails_schema =
    property $ do
        let schema = emptySchema{schemaType = Just SInteger}
        let op =
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
            Nothing -> assert False
            Just (_mediaType, payload) ->
                case eitherDecode (LBS.fromStrict payload) of
                    Left _ -> assert False
                    Right value ->
                        case value of
                            String _ -> success
                            _otherValue -> assert False

prop_negative_invalid_content_type_is_text_plain :: Property
prop_negative_invalid_content_type_is_text_plain =
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
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidContentType
        case reqBody mutated of
            Just (mediaType, _) -> assert (mediaType == "text/plain")
            Nothing -> assert False

prop_negative_invalid_content_type_preserves_payload :: Property
prop_negative_invalid_content_type_preserves_payload =
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
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidContentType
        case reqBody mutated of
            Just (_mediaType, payload) ->
                case eitherDecode (LBS.fromStrict payload) of
                    Left _ -> assert False
                    Right value ->
                        case value of
                            String _ -> success
                            _ -> assert False
            Nothing -> assert False

prop_negative_invalid_body_string_non_string :: Property
prop_negative_invalid_body_string_non_string =
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
        req <- forAll (genApiRequest op)
        let mutated = applyNegativeMutation op req InvalidRequestBody
        case reqBody mutated of
            Just (_mediaType, payload) -> do
                let decoded = eitherDecode (LBS.fromStrict payload) :: Either String Value
                case decoded of
                    Left _ -> assert False
                    Right val ->
                        case val of
                            String _ -> assert False
                            _ -> success
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
