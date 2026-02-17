module Haskemathesis.OpenApi.Loader (
    loadOpenApiFile,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)

import Data.OpenApi (OpenApi)

loadOpenApiFile :: FilePath -> IO (Either Text OpenApi)
loadOpenApiFile path = do
    result <- decodeFileEither path
    pure $ case result of
        Left err -> Left (T.pack (prettyPrintParseException err))
        Right spec -> Right spec
