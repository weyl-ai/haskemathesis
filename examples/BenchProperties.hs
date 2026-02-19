{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (foldl')
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskemathesis.Check.Standard (allChecks)
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.Property (propertiesForSpec, propertiesForSpecWithConfig)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    specResult <- loadOpenApiFile "examples/openapi-medium-spec.yaml"
    spec <- case specResult of
        Left err -> error (show err)
        Right s -> pure s
    let cfg = Nothing
        executor _timeout _req = pure (ApiResponse 200 [] "" 0)
        ops = resolveOperations spec
    start <- getCurrentTime
    let props = propertiesForSpec cfg allChecks executor ops
    end <- getCurrentTime
    hPutStrLn stderr ("generated properties: " <> show (foldl' (\n _ -> n + 1) (0 :: Int) props))
    hPutStrLn stderr ("generation time: " <> show (diffUTCTime end start))
