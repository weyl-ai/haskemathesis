{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Standard (
    defaultChecks,
    allChecks,
    notAServerError,
    responseSchemaConformance,
    statusCodeConformance,
    contentTypeConformance,
    responseHeadersConformance,
) where

import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import Network.HTTP.Types (HeaderName, hContentType)
import Text.Read (readMaybe)

import Haskemathesis.Check.Types
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse (..))
import Haskemathesis.OpenApi.Types
import Haskemathesis.Schema (Schema (..), SchemaType (..), emptySchema)
import Haskemathesis.Validate (validateErrors)

defaultChecks :: [Check]
defaultChecks =
    [ notAServerError
    , responseSchemaConformance
    , statusCodeConformance
    ]

allChecks :: [Check]
allChecks =
    defaultChecks
        <> [ contentTypeConformance
           , responseHeadersConformance
           ]

notAServerError :: Check
notAServerError =
    Check "not_a_server_error" $ \req res op ->
        if resStatusCode res >= 500
            then CheckFailed (failureDetail "not_a_server_error" "response status is 5xx" [] Nothing req res op)
            else CheckPassed

responseSchemaConformance :: Check
responseSchemaConformance =
    Check "response_schema_conformance" $ \req res op ->
        case findResponseSchema res op of
            Nothing -> CheckPassed
            Just schema ->
                case eitherDecode (LBS.fromStrict (resBody res)) of
                    Left err ->
                        CheckFailed
                            ( failureDetail
                                "response_schema_conformance"
                                ("response body is not valid JSON: " <> T.pack err)
                                []
                                Nothing
                                req
                                res
                                op
                            )
                    Right value ->
                        case validateErrors schema value of
                            [] -> CheckPassed
                            errs ->
                                let diff = schemaDiff schema value errs
                                 in CheckFailed
                                        ( failureDetail
                                            "response_schema_conformance"
                                            ("response violates schema: " <> T.intercalate "; " errs)
                                            errs
                                            (Just diff)
                                            req
                                            res
                                            op
                                        )

statusCodeConformance :: Check
statusCodeConformance =
    Check "status_code_conformance" $ \req res op ->
        if statusAllowed (resStatusCode res) op
            then CheckPassed
            else
                CheckFailed
                    ( failureDetail
                        "status_code_conformance"
                        ("response status code is not documented: " <> T.pack (show (resStatusCode res)))
                        []
                        Nothing
                        req
                        res
                        op
                    )

contentTypeConformance :: Check
contentTypeConformance =
    Check "content_type_conformance" $ \req res op ->
        case responseSchemasForStatus (resStatusCode res) op of
            Nothing -> CheckPassed
            Just responseSpec
                | Map.null (rsContent responseSpec) -> CheckPassed
                | otherwise ->
                    case lookupHeader hContentType (resHeaders res) of
                        Nothing ->
                            CheckFailed
                                ( failureDetail
                                    "content_type_conformance"
                                    "response is missing Content-Type header"
                                    []
                                    Nothing
                                    req
                                    res
                                    op
                                )
                        Just rawType ->
                            let contentType = decodeUtf8 rawType
                             in if matchesContentType contentType (rsContent responseSpec)
                                    then CheckPassed
                                    else
                                        CheckFailed
                                            ( failureDetail
                                                "content_type_conformance"
                                                ("response Content-Type not documented: " <> contentType)
                                                []
                                                Nothing
                                                req
                                                res
                                                op
                                            )

responseHeadersConformance :: Check
responseHeadersConformance =
    Check "response_headers_conformance" $ \req res op ->
        case responseSchemasForStatus (resStatusCode res) op of
            Nothing -> CheckPassed
            Just responseSpec ->
                case validateResponseHeaders responseSpec (resHeaders res) of
                    [] -> CheckPassed
                    errs ->
                        CheckFailed
                            ( failureDetail
                                "response_headers_conformance"
                                ("response headers invalid: " <> T.intercalate "; " errs)
                                []
                                Nothing
                                req
                                res
                                op
                            )

statusAllowed :: Int -> ResolvedOperation -> Bool
statusAllowed status op =
    Map.member status (roResponses op) || isJust (roDefaultResponse op)

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

normalizeContent :: Map Text Schema -> Map Text Schema
normalizeContent =
    Map.fromList . map (first normalizeMediaType) . Map.toList

normalizeMediaType :: Text -> Text
normalizeMediaType =
    T.toLower . T.strip . T.takeWhile (/= ';')

matchesContentType :: Text -> Map Text Schema -> Bool
matchesContentType contentType schemas =
    let responseType = normalizeMediaType contentType
        content = normalizeContent schemas
     in Map.member responseType content
            || jsonSuffixMatch responseType (Map.keys content)

lookupHeader :: HeaderName -> [(HeaderName, ByteString)] -> Maybe ByteString
lookupHeader = lookup

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

schemaDiff :: Schema -> Value -> [Text] -> Text
schemaDiff schema value errs =
    T.intercalate
        "\n"
        ( [ "Expected value to satisfy schema:"
          , "  type: " <> renderSchemaType (schemaType schema)
          ]
            <> map ("  - " <>) errs
            <> ["Actual value: " <> decodeUtf8 (LBS.toStrict (encode value))]
        )

renderSchemaType :: Maybe SchemaType -> Text
renderSchemaType mType =
    case mType of
        Nothing -> "unspecified"
        Just SString -> "string"
        Just SInteger -> "integer"
        Just SNumber -> "number"
        Just SBoolean -> "boolean"
        Just SArray -> "array"
        Just SObject -> "object"
        Just SNull -> "null"

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    fromMaybe (roMethod op <> " " <> roPath op) (roOperationId op)

normalizeHeader :: Text -> Text
normalizeHeader = T.toLower . T.strip

jsonSuffixMatch :: Text -> [Text] -> Bool
jsonSuffixMatch responseType declaredTypes =
    let isJsonLike t = T.isSuffixOf "+json" t || t == "application/json"
     in any (\declared -> isJsonLike responseType && isJsonLike declared) declaredTypes

validateResponseHeaders :: ResponseSpec -> [(HeaderName, ByteString)] -> [Text]
validateResponseHeaders responseSpec headers =
    missingErrors <> schemaErrors
  where
    headerValues = collectHeaderValues headers
    missing = filter (`Map.notMember` headerValues) (map normalizeHeader (rsRequiredHeaders responseSpec))
    missingErrors = ["missing required header: " <> name | name <- missing]
    schemaErrors =
        concatMap (validateHeaderSchema headerValues) (Map.toList (normalizeHeaderMap (rsHeaders responseSpec)))

validateHeaderSchema :: Map Text (NonEmpty ByteString) -> (Text, Schema) -> [Text]
validateHeaderSchema headerValues (name, schema) =
    case Map.lookup name headerValues of
        Nothing -> []
        Just raws ->
            case headerValueToJson schema raws of
                Left err -> ["header " <> name <> " invalid: " <> err]
                Right value ->
                    case validateErrors schema value of
                        [] -> []
                        errs ->
                            ["header " <> name <> " violates schema: " <> T.intercalate ", " errs]

normalizeHeaderMap :: Map Text Schema -> Map Text Schema
normalizeHeaderMap =
    Map.fromList . map (first normalizeHeader) . Map.toList

headerValueToJson :: Schema -> NonEmpty ByteString -> Either Text Value
headerValueToJson schema raws =
    case schemaType schema of
        Just SArray ->
            parseArray raws
        Just SString ->
            parseScalar schema (NE.head raws)
        Just SInteger ->
            parseScalar schema (NE.head raws)
        Just SNumber ->
            parseScalar schema (NE.head raws)
        Just SBoolean ->
            parseScalar schema (NE.head raws)
        Just SObject ->
            parseScalar schema (NE.head raws)
        Just SNull ->
            parseScalar schema (NE.head raws)
        Nothing ->
            parseScalar schema (NE.head raws)
  where
    parseArray values =
        let itemSchema = fromMaybe emptySchema (schemaItems schema)
            items =
                case values of
                    single :| [] -> splitCommaValues single
                    multipleValues -> NE.toList multipleValues
         in case traverse (parseScalar itemSchema) items of
                Left err -> Left err
                Right parsed -> Right (Array (Vector.fromList parsed))

parseScalar :: Schema -> ByteString -> Either Text Value
parseScalar schema raw =
    case schemaType schema of
        Just SInteger -> parseIntegral
        Just SNumber -> parseDouble
        Just SBoolean -> parseBool
        Just SNull ->
            if T.strip txt == ""
                then Right Null
                else Right (String txt)
        Just SString -> Right (String txt)
        Just SArray -> Right (String txt)
        Just SObject -> Right (String txt)
        Nothing -> Right (String txt)
  where
    txt = normalizeHeaderValue raw
    parseIntegral =
        case readMaybe (T.unpack txt) :: Maybe Integer of
            Nothing -> Left "expected integer"
            Just n -> Right (Number (fromIntegral n))
    parseDouble =
        case readMaybe (T.unpack txt) :: Maybe Double of
            Nothing -> Left "expected number"
            Just n -> Right (Number (fromFloatDigits n))
    parseBool =
        case T.toLower txt of
            "true" -> Right (Bool True)
            "false" -> Right (Bool False)
            _ -> Left "expected boolean"

normalizeHeaderValue :: ByteString -> Text
normalizeHeaderValue = stripQuotes . T.strip . decodeUtf8

stripQuotes :: Text -> Text
stripQuotes txt =
    case T.uncons txt of
        Just ('"', rest) | T.isSuffixOf "\"" rest -> T.dropEnd 1 rest
        Just ('\'', rest) | T.isSuffixOf "'" rest -> T.dropEnd 1 rest
        _unquoted -> txt

splitCommaValues :: ByteString -> [ByteString]
splitCommaValues raw =
    map encodeUtf8 (filter (not . T.null) (map T.strip (T.splitOn "," (decodeUtf8 raw))))

collectHeaderValues :: [(HeaderName, ByteString)] -> Map Text (NonEmpty ByteString)
collectHeaderValues headers =
    Map.fromListWith (<>) (map normalize headers)
  where
    normalize (name, value) =
        (normalizeHeader (decodeUtf8 (CI.original name)), value :| [])
