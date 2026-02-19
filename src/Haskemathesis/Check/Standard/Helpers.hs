{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Standard.Helpers (
    failureDetail,
    operationLabel,
    responseSchemasForStatus,
    findResponseSchema,
    pickSchema,
    normalizeMapKeys,
    normalizeContent,
    normalizeMediaType,
    matchesContentType,
    jsonSuffixMatch,
    lookupHeader,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (HeaderName, hContentType)

import Haskemathesis.Check.Types
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse (..))
import Haskemathesis.OpenApi.Types
import Haskemathesis.Schema (Schema)

failureDetail :: Text -> Text -> [Text] -> Maybe Text -> ApiRequest -> ApiResponse -> ResolvedOperation -> FailureDetail
failureDetail checkLabel message schemaErrors schemaDiffText req res op =
    FailureDetail
        { fdCheck = checkLabel
        , fdMessage = message
        , fdRequest = req
        , fdResponse = res
        , fdOperation = operationLabel op
        , fdSchemaErrors = schemaErrors
        , fdSchemaDiff = schemaDiffText
        , fdMutation = Nothing
        }

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    fromMaybe (roMethod op <> " " <> roPath op) (roOperationId op)

responseSchemasForStatus :: Int -> ResolvedOperation -> Maybe ResponseSpec
responseSchemasForStatus status op =
    case Map.lookup status (roResponses op) of
        Just schemas -> Just schemas
        Nothing -> roDefaultResponse op

findResponseSchema :: ApiResponse -> ResolvedOperation -> Maybe Schema
findResponseSchema res op = do
    responseSpec <- responseSchemasForStatus (resStatusCode res) op
    if Map.null (rsContent responseSpec)
        then Nothing
        else do
            let mContentType = decodeUtf8 <$> lookupHeader hContentType (resHeaders res)
            pickSchema mContentType (rsContent responseSpec)

pickSchema :: Maybe Text -> Map Text Schema -> Maybe Schema
pickSchema mContentType schemas =
    case mContentType of
        Nothing ->
            case Map.toList schemas of
                [(_mediaType, schema)] -> Just schema
                _otherSchemas -> Nothing
        Just contentType ->
            Map.lookup (normalizeMediaType contentType) (normalizeContent schemas)

-- | Normalize keys in a map using the given transformation function.
normalizeMapKeys :: (Text -> Text) -> Map Text a -> Map Text a
normalizeMapKeys f = Map.fromList . map (first f) . Map.toList

normalizeContent :: Map Text Schema -> Map Text Schema
normalizeContent = normalizeMapKeys normalizeMediaType

normalizeMediaType :: Text -> Text
normalizeMediaType =
    T.toLower . T.strip . T.takeWhile (/= ';')

matchesContentType :: Text -> Map Text Schema -> Bool
matchesContentType contentType schemas =
    let responseType = normalizeMediaType contentType
        content = normalizeContent schemas
     in Map.member responseType content
            || jsonSuffixMatch responseType (Map.keys content)

jsonSuffixMatch :: Text -> [Text] -> Bool
jsonSuffixMatch responseType declaredTypes =
    let isJsonLike t = T.isSuffixOf "+json" t || t == "application/json"
     in any (\declared -> isJsonLike responseType && isJsonLike declared) declaredTypes

lookupHeader :: HeaderName -> [(HeaderName, ByteString)] -> Maybe ByteString
lookupHeader = lookup
