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
-}
module Haskemathesis.OpenApi.Loader (
    loadOpenApiFile,
)
where

import Data.OpenApi (OpenApi)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)

{- | Load an OpenAPI specification from a file.

This function reads a YAML or JSON file and parses it into an
'OpenApi' value. It automatically detects the file format.

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
    result <- decodeFileEither path
    pure $ case result of
        Left err -> Left (T.pack (prettyPrintParseException err))
        Right spec -> Right spec
