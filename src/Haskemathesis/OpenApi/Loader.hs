{-# LANGUAGE OverloadedStrings #-}

{- | Load OpenAPI specs from YAML or JSON files.

This module provides functions for loading OpenAPI specifications
from disk. It supports both YAML and JSON formats, automatically
detecting the format based on file extension and content.

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

=== Error Handling

The loader returns 'Either Text OpenApi', where the Left case
contains a descriptive error message if parsing fails. This
can happen due to:

* File not found or unreadable
* Invalid YAML/JSON syntax
* Invalid OpenAPI structure (validation errors)

=== Supported Formats

* YAML files (.yaml, .yml extension)
* JSON files (.json extension)
* Files without extension (auto-detected by content)

=== OpenAPI 3.1 Support

This loader automatically transforms OpenAPI 3.1 constructs to
their OpenAPI 3.0 equivalents before parsing. This includes:

* @exclusiveMinimum: \<number\>@ → @minimum: \<number\>, exclusiveMinimum: true@
* @exclusiveMaximum: \<number\>@ → @maximum: \<number\>, exclusiveMaximum: true@

This allows specs written for OpenAPI 3.1 (which uses JSON Schema 2020-12)
to be loaded by the openapi3 library which only supports OpenAPI 3.0.
-}
module Haskemathesis.OpenApi.Loader (
    loadOpenApiFile,

    -- * Internal (exported for testing)
    transformOpenApi31To30,
)
where

import Data.Aeson (Result (..), Value (..), fromJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import Data.OpenApi (OpenApi)
import Data.Scientific (isInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml (decodeFileEither, prettyPrintParseException)

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
