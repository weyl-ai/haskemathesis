{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Standard.Headers (responseHeadersConformance) where

import Data.Aeson (Value (..))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as Vector
import Network.HTTP.Types (HeaderName)
import Text.Read (readMaybe)

import Haskemathesis.Check.Standard.Helpers (failureDetail, responseSchemasForStatus)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResponseSpec (..))
import Haskemathesis.Schema (Schema (..), SchemaType (..), emptySchema)
import Haskemathesis.Validate (validateErrors)

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

normalizeHeader :: Text -> Text
normalizeHeader = T.toLower . T.strip
