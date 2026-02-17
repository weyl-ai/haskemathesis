{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskemathesis.Check.Standard (allChecks)
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.Property (propertiesForSpec)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    specResult <- loadOpenApiFile "examples/medium-spec.yaml"
    spec <- case specResult of
        Left err -> error (show err)
        Right s -> pure s
    let cfg = Nothing
        executor _ = pure (ApiResponse 200 [] "" 0)
        ops = resolveOperations spec
    start <- getCurrentTime
    let props = propertiesForSpec cfg allChecks executor ops
    end <- getCurrentTime
    hPutStrLn stderr ("generated properties: " <> show (length props))
    hPutStrLn stderr ("generation time: " <> show (diffUTCTime end start))
