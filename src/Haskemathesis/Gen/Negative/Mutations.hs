{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Gen.Negative.Mutations
Description : Request mutation operations for negative testing
Stability   : experimental

This module provides functions for mutating valid API requests into
invalid ones for negative testing. It generates various types of
malformed requests to verify that the API correctly rejects invalid input.
-}
module Haskemathesis.Gen.Negative.Mutations (
    mutationCandidates,
    applyNegativeMutation,
)
where

import Data.Aeson (Value (..), encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Haskemathesis.Execute.Types (ApiRequest (..), MediaType)
import Haskemathesis.Gen.Negative.Types (NegativeMutation (..))
import Haskemathesis.OpenApi.Types (ParamLocation (..), ResolvedOperation (..), ResolvedParam (..), ResolvedRequestBody (..))
import Haskemathesis.Schema (Schema (..), SchemaType (..))

{- | Apply a specific negative mutation to an API request.

Given a valid request and a mutation type, this function produces an
intentionally invalid request that can be used to test error handling.

==== Parameters

* @op@ - The resolved operation (needed for schema and path information)
* @req@ - The original valid request to mutate
* @mutation@ - The type of mutation to apply

==== Mutation types

* 'RemoveRequiredPath' - Removes a required path parameter
* 'RemoveRequiredHeader' - Removes a required header
* 'RemoveRequiredQuery' - Removes a required query parameter
* 'InvalidPathParam' - Sets a path parameter to an invalid value
* 'InvalidHeader' - Sets a header to an invalid value
* 'InvalidQueryParam' - Sets a query parameter to an invalid value
* 'InvalidRequestBody' - Replaces the body with an invalid value
* 'InvalidContentType' - Changes the Content-Type to an undocumented type

==== Example

@
let invalidReq = applyNegativeMutation op validReq (RemoveRequiredHeader "Authorization")
-- invalidReq now has no Authorization header
@
-}
applyNegativeMutation :: ResolvedOperation -> ApiRequest -> NegativeMutation -> ApiRequest
applyNegativeMutation op req mutation =
    case mutation of
        RemoveRequiredPath name -> dropPathParam op req name
        RemoveRequiredHeader name -> dropHeader name req
        RemoveRequiredQuery name -> dropQuery name req
        InvalidPathParam name ->
            case lookupParam ParamPath name op of
                Just schema -> setPathParam op req name (invalidFor schema)
                Nothing -> req
        InvalidHeader name ->
            case lookupParam ParamHeader name op of
                Just schema -> setHeaderValue name (invalidFor schema) req
                Nothing -> req
        InvalidQueryParam name ->
            case lookupParam ParamQuery name op of
                Just schema -> setQueryValue name (invalidFor schema) req
                Nothing -> req
        InvalidRequestBody ->
            case roRequestBody op of
                Just body -> setInvalidBody body req
                Nothing -> req
        InvalidContentType ->
            case roRequestBody op of
                Just body -> setContentType body "text/plain" req
                Nothing -> req
  where
    invalidFor schema = fromMaybe "not-a-value" (invalidValueText schema)

mutationCandidates :: ResolvedOperation -> ApiRequest -> [(NegativeMutation, ApiRequest -> ApiRequest)]
mutationCandidates op req =
    concat
        [ removeRequiredPaths op req
        , removeRequiredHeaders op
        , removeRequiredQueries op
        , invalidPathParams op req
        , invalidHeaders op
        , invalidQueries op
        , invalidBody op
        , invalidContentType op req
        ]

-- | Get required parameters at a given location.
requiredParamsAt :: ParamLocation -> ResolvedOperation -> [ResolvedParam]
requiredParamsAt location op =
    [param | param <- roParameters op, rpLocation param == location, rpRequired param]

-- | Get all parameters at a given location.
paramsAt :: ParamLocation -> ResolvedOperation -> [ResolvedParam]
paramsAt location op =
    [param | param <- roParameters op, rpLocation param == location]

removeRequiredPaths :: ResolvedOperation -> ApiRequest -> [(NegativeMutation, ApiRequest -> ApiRequest)]
removeRequiredPaths op req =
    [ (RemoveRequiredPath (rpName param), const (dropPathParam op req (rpName param)))
    | param <- requiredParamsAt ParamPath op
    ]

removeRequiredHeaders :: ResolvedOperation -> [(NegativeMutation, ApiRequest -> ApiRequest)]
removeRequiredHeaders op =
    [ (RemoveRequiredHeader (rpName param), dropHeader (rpName param))
    | param <- requiredParamsAt ParamHeader op
    ]

removeRequiredQueries :: ResolvedOperation -> [(NegativeMutation, ApiRequest -> ApiRequest)]
removeRequiredQueries op =
    [ (RemoveRequiredQuery (rpName param), dropQuery (rpName param))
    | param <- requiredParamsAt ParamQuery op
    ]

invalidPathParams :: ResolvedOperation -> ApiRequest -> [(NegativeMutation, ApiRequest -> ApiRequest)]
invalidPathParams op req =
    [ (InvalidPathParam (rpName param), const (setPathParam op req (rpName param) invalidValue))
    | param <- paramsAt ParamPath op
    , Just invalidValue <- [invalidValueText (rpSchema param)]
    ]

invalidHeaders :: ResolvedOperation -> [(NegativeMutation, ApiRequest -> ApiRequest)]
invalidHeaders op =
    [ (InvalidHeader (rpName param), setHeaderValue (rpName param) invalidValue)
    | param <- paramsAt ParamHeader op
    , Just invalidValue <- [invalidValueText (rpSchema param)]
    ]

invalidQueries :: ResolvedOperation -> [(NegativeMutation, ApiRequest -> ApiRequest)]
invalidQueries op =
    [ (InvalidQueryParam (rpName param), setQueryValue (rpName param) invalidValue)
    | param <- paramsAt ParamQuery op
    , Just invalidValue <- [invalidValueText (rpSchema param)]
    ]

invalidBody :: ResolvedOperation -> [(NegativeMutation, ApiRequest -> ApiRequest)]
invalidBody op =
    case roRequestBody op of
        Nothing -> []
        Just body ->
            [(InvalidRequestBody, setInvalidBody body)]

invalidContentType :: ResolvedOperation -> ApiRequest -> [(NegativeMutation, ApiRequest -> ApiRequest)]
invalidContentType op req =
    case roRequestBody op of
        Nothing -> []
        Just body ->
            case reqBody req of
                Nothing -> []
                Just _ -> [(InvalidContentType, setContentType body "text/plain")]

setInvalidBody :: ResolvedRequestBody -> ApiRequest -> ApiRequest
setInvalidBody body req =
    let invalidValue = invalidValueFor (rbSchema body)
        payload = LBS.toStrict (encode invalidValue)
     in req{reqBody = Just (rbContentType body, payload)}

setContentType :: ResolvedRequestBody -> MediaType -> ApiRequest -> ApiRequest
setContentType body mediaType req =
    case reqBody req of
        Nothing -> req{reqBody = Just (mediaType, LBS.toStrict (encode (invalidValueFor (rbSchema body))))}
        Just (_oldType, payload) -> req{reqBody = Just (mediaType, payload)}

invalidValueFor :: Schema -> Value
invalidValueFor schema =
    case schemaType schema of
        Just SString -> Number 1
        Just SInteger -> String "oops"
        Just SNumber -> String "oops"
        Just SBoolean -> String "oops"
        Just SArray -> String "oops"
        Just SObject -> String "oops"
        Just SNull -> String "oops"
        Nothing -> String "oops"

invalidValueText :: Schema -> Maybe Text
invalidValueText schema =
    case schemaEnum schema of
        Just enums ->
            let values = [txt | String txt <- enums]
             in Just (invalidEnumValue values)
        Nothing ->
            case schemaType schema of
                Just SBoolean -> Just "not-a-boolean"
                Just SInteger -> Just "not-a-number"
                Just SNumber -> Just "not-a-number"
                Just SString -> Nothing
                Just SArray -> Just "not-a-array"
                Just SObject -> Just "not-a-object"
                Just SNull -> Just "not-null"
                Nothing -> Just "not-a-value"

invalidEnumValue :: [Text] -> Text
invalidEnumValue values =
    let base = "not-in-enum"
     in if base `elem` values
            then base <> "-1"
            else base

setHeaderValue :: Text -> Text -> ApiRequest -> ApiRequest
setHeaderValue name value req =
    let headerName = CI.mk (encodeUtf8 name)
        newValue = encodeUtf8 value
     in req{reqHeaders = replaceAssoc headerName newValue (reqHeaders req)}

setQueryValue :: Text -> Text -> ApiRequest -> ApiRequest
setQueryValue name value req =
    req{reqQueryParams = replaceAssoc name value (reqQueryParams req)}

-- | Replace or insert a value in an association list.
replaceAssoc :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
replaceAssoc key val assocs =
    case find ((== key) . fst) assocs of
        Nothing -> (key, val) : assocs
        Just _ -> map (\(k, v) -> if k == key then (k, val) else (k, v)) assocs

dropHeader :: Text -> ApiRequest -> ApiRequest
dropHeader name req =
    let headerName = CI.mk (encodeUtf8 name)
     in req{reqHeaders = filter ((/= headerName) . fst) (reqHeaders req)}

dropQuery :: Text -> ApiRequest -> ApiRequest
dropQuery name req =
    req{reqQueryParams = filter ((/= name) . fst) (reqQueryParams req)}

dropPathParam :: ResolvedOperation -> ApiRequest -> Text -> ApiRequest
dropPathParam op req name =
    setPathParam op req name ""

setPathParam :: ResolvedOperation -> ApiRequest -> Text -> Text -> ApiRequest
setPathParam op req name value =
    let paramMap = extractPathParams (roPath op) (reqPath req)
        path = buildPath (roPath op) (Map.insert name value paramMap)
     in req{reqPath = path}

extractPathParams :: Text -> Text -> Map.Map Text Text
extractPathParams template path =
    let templateSegs = T.splitOn "/" template
        pathSegs = T.splitOn "/" path
     in Map.fromList
            [ (paramName, pathSeg)
            | (templateSeg, pathSeg) <- zip templateSegs pathSegs
            , Just paramName <- [templateParamName templateSeg]
            ]

buildPath :: Text -> Map.Map Text Text -> Text
buildPath template paramMap =
    T.intercalate "/" (map renderSeg (T.splitOn "/" template))
  where
    renderSeg seg =
        case templateParamName seg of
            Nothing -> seg
            Just name -> Map.findWithDefault "missing" name paramMap

templateParamName :: Text -> Maybe Text
templateParamName segment =
    if T.isPrefixOf "{" segment && T.isSuffixOf "}" segment
        then Just (T.drop 1 (T.dropEnd 1 segment))
        else Nothing

lookupParam :: ParamLocation -> Text -> ResolvedOperation -> Maybe Schema
lookupParam location name op =
    case filter match (roParameters op) of
        [] -> Nothing
        (param : _) -> Just (rpSchema param)
  where
    match param = rpLocation param == location && rpName param == name
