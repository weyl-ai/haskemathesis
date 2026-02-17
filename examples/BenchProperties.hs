{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Haskemathesis.Check.Standard (allChecks)
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.Property (propertiesForSpecWithConfig)
import System.IO (hPutStrLn, stderr)

main :: IO ()
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
    start <- getCurrentTime
    let props = propertiesForSpecWithConfig spec cfg executor ops
    end <- getCurrentTime
    let totalProperties = length props * tcPropertyCount cfg
    hPutStrLn stderr ("generated properties: " <> show totalProperties)
    hPutStrLn stderr ("generation time: " <> show (diffUTCTime end start))
