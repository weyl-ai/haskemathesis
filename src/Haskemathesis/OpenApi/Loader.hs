{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | Load OpenAPI specs from YAML or JSON files or URLs.

This module provides functions for loading OpenAPI specifications
from disk or remote URLs. It supports both YAML and JSON formats,
automatically detecting the format based on file extension and content.

=== Basic Usage

Load a specification from a file:

@
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)

main :: IO ()
main = do
    result <- loadOpenApiFile "api.yaml"
    case result of
        Left err -> putStrLn $ "Failed to load: " ++ show err
        Right spec -> putStrLn "Loaded successfully!"
@

=== Loading from URLs

You can also load specifications from URLs:

@
import Haskemathesis.OpenApi.Loader (loadOpenApi)

main :: IO ()
main = do
    result <- loadOpenApi "https://api.example.com/openapi.yaml"
    case result of
        Left err -> putStrLn $ "Failed to load: " ++ show err
        Right spec -> putStrLn "Loaded from URL!"
@

=== Error Handling

The loader returns 'Either Text OpenApi', where the Left case
contains a descriptive error message if parsing fails. This
can happen due to:

* File not found or unreadable
* Network error when fetching from URL
* Invalid YAML/JSON syntax
* Invalid OpenAPI structure (validation errors)

=== Supported Formats

* YAML files (.yaml, .yml extension)
* JSON files (.json extension)
* Files without extension (auto-detected by content)
* HTTP/HTTPS URLs

=== OpenAPI 3.1 Support

This loader automatically transforms OpenAPI 3.1 constructs to
their OpenAPI 3.0 equivalents before parsing. This includes:

* @exclusiveMinimum: \<number\>@ → @minimum: \<number\>, exclusiveMinimum: true@
* @exclusiveMaximum: \<number\>@ → @maximum: \<number\>, exclusiveMaximum: true@

This allows specs written for OpenAPI 3.1 (which uses JSON Schema 2020-12)
to be loaded by the openapi3 library which only supports OpenAPI 3.0.
-}
module Haskemathesis.OpenApi.Loader (
    -- * Loading functions
    loadOpenApi,
    loadOpenApiFile,
    loadOpenApiUrl,
    loadOpenApiFileWithExtensions,
    OperationExtensions (..),
    OperationKey,

    -- * URL detection
    isUrl,

    -- * Internal (exported for testing)
    transformOpenApi31To30,
    extractOperationExtensions,
)
where

import Control.Exception (SomeException, catch)
import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.OpenApi (OpenApi)
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (decodeEither', decodeFileEither, prettyPrintParseException)
import Network.HTTP.Client (
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
 )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

{- | Load an OpenAPI specification from a file.

This function reads a YAML or JSON file and parses it into an
'OpenApi' value. It automatically detects the file format.

OpenAPI 3.1 specs are automatically transformed to OpenAPI 3.0
format before parsing, allowing compatibility with the openapi3 library.

=== Parameters

* @path@ - Path to the OpenAPI specification file

=== Return Value

Returns 'Right OpenApi' on success, or 'Left Text' with an error
message on failure.

=== Example

@
main :: IO ()
main = do
    result <- loadOpenApiFile "specs/api.yaml"
    case result of
        Left err -> putStrLn $ "Error: " ++ T.unpack err
        Right spec -> do
            putStrLn $ "Loaded API with " ++ show (length spec) ++ " paths"
@
-}

{- | Load an OpenAPI specification from a file path or URL.

This is the recommended entry point for loading specs. It automatically
detects whether the input is a URL (starting with @http://@ or @https://@)
or a file path, and uses the appropriate loading method.

=== Parameters

* @pathOrUrl@ - Either a file path or a URL to the OpenAPI specification

=== Return Value

Returns 'Right OpenApi' on success, or 'Left Text' with an error
message on failure.

=== Example

@
-- Load from file
spec1 <- loadOpenApi "api.yaml"

-- Load from URL
spec2 <- loadOpenApi "https://petstore.swagger.io/v2/swagger.json"
@
-}
loadOpenApi :: String -> IO (Either Text OpenApi)
loadOpenApi pathOrUrl
    | isUrl pathOrUrl = loadOpenApiUrl pathOrUrl
    | otherwise = loadOpenApiFile pathOrUrl

-- | Check if a string is a URL (starts with http:// or https://).
isUrl :: String -> Bool
isUrl s = "http://" `isPrefixOf` s || "https://" `isPrefixOf` s

loadOpenApiFile :: FilePath -> IO (Either Text OpenApi)
loadOpenApiFile path = do
    -- First parse as raw Value to allow transformation
    result <- decodeFileEither path
    pure $ case result of
        Left err -> Left (T.pack (prettyPrintParseException err))
        Right value ->
            -- Transform 3.1 constructs to 3.0, then decode to OpenApi
            let transformed = transformOpenApi31To30 value
             in case fromJSON transformed of
                    Error err' -> Left (T.pack err')
                    Success spec -> Right spec

{- | Load an OpenAPI specification from a URL.

This function fetches an OpenAPI specification from an HTTP or HTTPS URL.
It supports both YAML and JSON formats.

=== Parameters

* @url@ - URL to the OpenAPI specification (must start with http:// or https://)

=== Return Value

Returns 'Right OpenApi' on success, or 'Left Text' with an error
message on failure.

=== Example

@
result <- loadOpenApiUrl "https://petstore.swagger.io/v2/swagger.json"
case result of
    Left err -> putStrLn $ "Failed to fetch: " ++ T.unpack err
    Right spec -> putStrLn "Loaded from URL!"
@

=== Notes

* Uses TLS for HTTPS connections
* Follows redirects automatically
* Returns an error if the response status is not 2xx
-}
loadOpenApiUrl :: String -> IO (Either Text OpenApi)
loadOpenApiUrl url = do
    result <- fetchUrl url
    pure $ case result of
        Left err -> Left err
        Right body ->
            -- Parse as YAML (which is a superset of JSON)
            case decodeEither' (LBS.toStrict body) of
                Left err -> Left (T.pack (prettyPrintParseException err))
                Right value ->
                    let transformed = transformOpenApi31To30 value
                     in case fromJSON transformed of
                            Error err' -> Left (T.pack err')
                            Success spec -> Right spec

-- | Fetch content from a URL.
fetchUrl :: String -> IO (Either Text LBS.ByteString)
fetchUrl url =
    ( do
        manager <- newManager tlsManagerSettings
        request <- parseRequest url
        response <- httpLbs request manager
        let status = statusCode (responseStatus response)
        if status >= 200 && status < 300
            then pure $ Right (responseBody response)
            else pure $ Left $ "HTTP error: status " <> T.pack (show status)
    )
        `catch` handleHttpException
  where
    handleHttpException :: SomeException -> IO (Either Text LBS.ByteString)
    handleHttpException e = pure $ Left $ "Failed to fetch URL: " <> T.pack (show e)

{- | Transform OpenAPI 3.1 JSON Schema constructs to OpenAPI 3.0 equivalents.

This function recursively walks the JSON structure and transforms:

* OpenAPI version @3.1.x@ to @3.0.3@ (highest supported by openapi3 library)
* Numeric @exclusiveMinimum@ to @minimum@ + boolean @exclusiveMinimum: true@
* Numeric @exclusiveMaximum@ to @maximum@ + boolean @exclusiveMaximum: true@

This allows OpenAPI 3.1 specs (which use JSON Schema 2020-12 semantics)
to be parsed by libraries that only support OpenAPI 3.0 (JSON Schema draft-04).
-}
transformOpenApi31To30 :: Value -> Value
transformOpenApi31To30 = transformValue
  where
    transformValue :: Value -> Value
    transformValue (Object obj) = Object (transformObject obj)
    transformValue (Array arr) = Array (V.map transformValue arr)
    transformValue other = other

    transformObject :: KM.KeyMap Value -> KM.KeyMap Value
    transformObject obj =
        let
            -- First recursively transform all nested values
            recursed = KM.map transformValue obj
            -- Transform OpenAPI version 3.1.x to 3.0.3
            withVersion = transformOpenapiVersion recursed
            -- Then apply exclusiveMinimum/Maximum transformations
            withExclMin = transformExclusiveMinimum withVersion
            withExclMax = transformExclusiveMaximum withExclMin
         in
            withExclMax

    -- Transform openapi: "3.1.x" to "3.0.3"
    transformOpenapiVersion :: KM.KeyMap Value -> KM.KeyMap Value
    transformOpenapiVersion obj =
        case KM.lookup openapiKey obj of
            Just (String ver)
                | T.isPrefixOf "3.1" ver ->
                    KM.insert openapiKey (String "3.0.3") obj
            _notOpenApi31 -> obj

    -- Transform exclusive<Bound>: <number> to <bound>: <number>, exclusive<Bound>: true
    transformExclusiveBound :: Key -> Key -> KM.KeyMap Value -> KM.KeyMap Value
    transformExclusiveBound exclusiveKey boundKey obj =
        case KM.lookup exclusiveKey obj of
            Just (Number n)
                | isInteger n || otherwise ->
                    -- Remove the numeric exclusive bound
                    -- Add bound with the value, and exclusive bound: true
                    let withoutOld = KM.delete exclusiveKey obj
                        withBound = KM.insert boundKey (Number n) withoutOld
                        withExcl = KM.insert exclusiveKey (Bool True) withBound
                     in withExcl
            _notNumericExcl -> obj

    transformExclusiveMinimum :: KM.KeyMap Value -> KM.KeyMap Value
    transformExclusiveMinimum = transformExclusiveBound "exclusiveMinimum" "minimum"

    transformExclusiveMaximum :: KM.KeyMap Value -> KM.KeyMap Value
    transformExclusiveMaximum = transformExclusiveBound "exclusiveMaximum" "maximum"

    openapiKey :: Key
    openapiKey = "openapi"

{- | Vendor extensions extracted from an OpenAPI operation.

These extensions are non-standard fields (prefixed with @x-@) that
Haskemathesis recognizes for controlling test behavior.

=== Supported Extensions

[@x-timeout@] Request timeout in milliseconds. Useful for:

    * Streaming endpoints (SSE, NDJSON) that never complete
    * Slow endpoints that need longer than default timeout
    * Fast endpoints where you want to fail quickly

    Example in OpenAPI spec:

    @
    \/events:
      get:
        x-timeout: 2000  # 2 second timeout
        responses:
          "200":
            content:
              text/event-stream: {}
    @

=== Why Use Extensions?

OpenAPI 3.x doesn't have a standard way to specify:

* Request timeouts
* Streaming behavior hints
* Test-specific configuration

Vendor extensions (x-* fields) are the OpenAPI-sanctioned way to add
such metadata while remaining spec-compliant.
-}
newtype OperationExtensions = OperationExtensions
    { oeTimeout :: Maybe Int
    -- ^ Request timeout in milliseconds from @x-timeout@
    }
    deriving (Eq, Show)

-- | Key for looking up operation extensions: (path, method)
type OperationKey = (Text, Text)

{- | Load an OpenAPI spec along with vendor extensions.

This function returns both the parsed OpenAPI spec and a map of
vendor extensions for each operation, keyed by (path, method).

=== Example

@
result <- loadOpenApiFileWithExtensions "api.yaml"
case result of
    Left err -> putStrLn $ "Error: " ++ T.unpack err
    Right (spec, extensions) -> do
        -- Look up timeout for GET /events
        let key = ("/events", "GET")
        case Map.lookup key extensions of
            Just ext -> print (oeTimeout ext)
            Nothing -> putStrLn "No extensions"
@
-}
loadOpenApiFileWithExtensions ::
    FilePath ->
    IO (Either Text (OpenApi, Map OperationKey OperationExtensions))
loadOpenApiFileWithExtensions path = do
    result <- decodeFileEither path
    pure $ case result of
        Left err -> Left (T.pack (prettyPrintParseException err))
        Right value ->
            let transformed = transformOpenApi31To30 value
                extensions = extractOperationExtensions value
             in case fromJSON transformed of
                    Error err' -> Left (T.pack err')
                    Success spec -> Right (spec, extensions)

{- | Extract vendor extensions from all operations in a raw OpenAPI JSON value.

This function walks the paths and operations in the raw JSON to extract
x-* fields before they are discarded by the openapi3 library parser.

Returns a map from (path, method) to the extracted extensions.
-}
extractOperationExtensions :: Value -> Map OperationKey OperationExtensions
extractOperationExtensions (Object root) =
    case KM.lookup "paths" root of
        Just (Object paths) ->
            Map.fromList
                [ ((path, method), ext)
                | (pathKey, Object pathItem) <- KM.toList paths
                , let path = Key.toText pathKey
                , (methodKey, Object operation) <- KM.toList pathItem
                , let method = T.toUpper (Key.toText methodKey)
                , method `elem` httpMethods
                , let ext = extractExtensionsFromOperation operation
                ]
        _noPaths -> Map.empty
extractOperationExtensions _notObject = Map.empty

-- | HTTP methods to look for in path items
httpMethods :: [Text]
httpMethods = ["GET", "PUT", "POST", "DELETE", "OPTIONS", "HEAD", "PATCH", "TRACE"]

-- | Extract extensions from a single operation object
extractExtensionsFromOperation :: KM.KeyMap Value -> OperationExtensions
extractExtensionsFromOperation op =
    OperationExtensions
        { oeTimeout = extractTimeout op
        }

-- | Extract x-timeout as milliseconds (Int)
extractTimeout :: KM.KeyMap Value -> Maybe Int
extractTimeout op =
    case KM.lookup "x-timeout" op of
        Just (Number n) -> toBoundedInteger n
        _notNumber -> Nothing
