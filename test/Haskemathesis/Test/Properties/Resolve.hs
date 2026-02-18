{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Resolve (spec) where

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map.Strict as Map
import Data.OpenApi (
    MediaTypeObject (..),
    OpenApi (..),
    Operation (..),
    Param (..),
    ParamLocation (..),
    PathItem (..),
    Reference (..),
    Referenced (..),
    Response (..),
    Responses (..),
    Schema,
 )
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import qualified Haskemathesis.OpenApi.Types as HOT
import Haskemathesis.Test.Support (itProp)
import Hedgehog (Property, assert, property, (===))
import Test.Hspec (Spec, describe)

-- | Simple string schema for testing
stringSchema :: Schema
stringSchema = mempty

spec :: Spec
spec =
    describe "OpenAPI Resolution" $ do
        itProp "missing ref filtered out" prop_resolve_ref_missing_filtered
        itProp "operation extracts all params" prop_resolve_operation_extracts_all_params
        itProp "response codes preserved" prop_resolve_response_codes_preserved
        itProp "content types preserved" prop_resolve_content_types_preserved
        itProp "path and operation params merged" prop_resolve_params_merged

-- | Missing $ref references should result in the item being filtered out
prop_resolve_ref_missing_filtered :: Property
prop_resolve_ref_missing_filtered =
    property $ do
        -- Operation with a parameter that references a missing component
        let missingParamRef = Ref (Reference "#/components/parameters/NonExistent")
            op =
                mempty
                    { _operationParameters = [missingParamRef]
                    , _operationResponses = mempty
                    }
            pathItem = mempty{_pathItemGet = Just op}
            openApi = mempty{_openApiPaths = InsOrdHashMap.fromList [("/test", pathItem)]}
            resolved = resolveOperations openApi
        -- Operation should be resolved but with no parameters (missing ref filtered)
        length resolved === 1
        case resolved of
            [resolvedOp] -> HOT.roParameters resolvedOp === []
            _ -> assert False

-- | Resolution should extract all parameters from an operation
prop_resolve_operation_extracts_all_params :: Property
prop_resolve_operation_extracts_all_params =
    property $ do
        let queryParam =
                mempty
                    { _paramName = "limit"
                    , _paramIn = ParamQuery
                    , _paramRequired = Just False
                    , _paramSchema = Just (Inline stringSchema)
                    }
            headerParam =
                mempty
                    { _paramName = "X-Request-Id"
                    , _paramIn = ParamHeader
                    , _paramRequired = Just True
                    , _paramSchema = Just (Inline stringSchema)
                    }
            op =
                mempty
                    { _operationParameters = [Inline queryParam, Inline headerParam]
                    , _operationResponses = mempty
                    }
            pathItem = mempty{_pathItemGet = Just op}
            openApi = mempty{_openApiPaths = InsOrdHashMap.fromList [("/items", pathItem)]}
            resolved = resolveOperations openApi
        length resolved === 1
        case resolved of
            [resolvedOp] -> do
                length (HOT.roParameters resolvedOp) === 2
                let paramNames = map HOT.rpName (HOT.roParameters resolvedOp)
                assert ("limit" `elem` paramNames)
                assert ("X-Request-Id" `elem` paramNames)
            _ -> assert False

-- | Response status codes should be preserved during resolution
prop_resolve_response_codes_preserved :: Property
prop_resolve_response_codes_preserved =
    property $ do
        let response200 = mempty :: Response
            response404 = mempty :: Response
            response500 = mempty :: Response
            responses =
                mempty
                    { _responsesResponses =
                        InsOrdHashMap.fromList
                            [ (200, Inline response200)
                            , (404, Inline response404)
                            , (500, Inline response500)
                            ]
                    }
            op = mempty{_operationResponses = responses}
            pathItem = mempty{_pathItemGet = Just op}
            openApi = mempty{_openApiPaths = InsOrdHashMap.fromList [("/resource", pathItem)]}
            resolved = resolveOperations openApi
        length resolved === 1
        case resolved of
            [resolvedOp] -> do
                let responseCodes = Map.keys (HOT.roResponses resolvedOp)
                assert (200 `elem` responseCodes)
                assert (404 `elem` responseCodes)
                assert (500 `elem` responseCodes)
                length responseCodes === 3
            _ -> assert False

-- | Content types should be preserved in response schemas
prop_resolve_content_types_preserved :: Property
prop_resolve_content_types_preserved =
    property $ do
        let jsonMedia =
                mempty
                    { _mediaTypeObjectSchema = Just (Inline stringSchema)
                    }
            xmlMedia =
                mempty
                    { _mediaTypeObjectSchema = Just (Inline stringSchema)
                    }
            response =
                mempty
                    { _responseContent =
                        InsOrdHashMap.fromList
                            [ ("application/json", jsonMedia)
                            , ("application/xml", xmlMedia)
                            ]
                    }
            responses = mempty{_responsesResponses = InsOrdHashMap.fromList [(200, Inline response)]}
            op = mempty{_operationResponses = responses}
            pathItem = mempty{_pathItemGet = Just op}
            openApi = mempty{_openApiPaths = InsOrdHashMap.fromList [("/data", pathItem)]}
            resolved = resolveOperations openApi
        length resolved === 1
        case resolved of
            [resolvedOp] -> do
                case Map.lookup 200 (HOT.roResponses resolvedOp) of
                    Just responseSpec -> do
                        let contentTypes = Map.keys (HOT.rsContent responseSpec)
                        assert ("application/json" `elem` contentTypes)
                        assert ("application/xml" `elem` contentTypes)
                    Nothing -> assert False
            _ -> assert False

-- | Path-level and operation-level parameters should be merged
prop_resolve_params_merged :: Property
prop_resolve_params_merged =
    property $ do
        let pathParam =
                mempty
                    { _paramName = "id"
                    , _paramIn = ParamPath
                    , _paramRequired = Just True
                    , _paramSchema = Just (Inline stringSchema)
                    }
            queryParam =
                mempty
                    { _paramName = "format"
                    , _paramIn = ParamQuery
                    , _paramRequired = Just False
                    , _paramSchema = Just (Inline stringSchema)
                    }
            op =
                mempty
                    { _operationParameters = [Inline queryParam]
                    , _operationResponses = mempty
                    }
            pathItem =
                mempty
                    { _pathItemGet = Just op
                    , _pathItemParameters = [Inline pathParam]
                    }
            openApi = mempty{_openApiPaths = InsOrdHashMap.fromList [("/items/{id}", pathItem)]}
            resolved = resolveOperations openApi
        length resolved === 1
        case resolved of
            [resolvedOp] -> do
                length (HOT.roParameters resolvedOp) === 2
                let paramNames = map HOT.rpName (HOT.roParameters resolvedOp)
                assert ("id" `elem` paramNames)
                assert ("format" `elem` paramNames)
            _ -> assert False
