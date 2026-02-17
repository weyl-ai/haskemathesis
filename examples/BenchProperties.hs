{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskemathesis.Config (defaultTestConfig)
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.Property (propertiesForSpec)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    spec <- loadOpenApiFile "examples/medium-spec.yaml"
    let cfg = defaultTestConfig
        executor _ = pure (ApiResponse 200 [] "" 0)
    start <- getCurrentTime
    let props = propertiesForSpec cfg spec executor
    end <- getCurrentTime
    hPutStrLn stderr ("generated properties: " <> show (length props))
    hPutStrLn stderr ("generation time: " <> show (diffUTCTime end start))
