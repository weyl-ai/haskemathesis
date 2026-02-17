{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Request (spec) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, assert, failure, forAll, property, (===))
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types
import Haskemathesis.Schema
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "Request generation" $ do
        itProp "path interpolation" prop_request_path_interpolation
        itProp "path interpolation with multiple params" prop_request_path_multiple_params
        itProp "header generation" prop_request_header_generation
        itProp "request body sets media type" prop_request_body_sets_media_type

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
