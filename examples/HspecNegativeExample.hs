{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Types (status400)
import Network.Wai (Application, responseLBS)
import Test.Hspec (hspec)

import Data.OpenApi (OpenApi)

import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Integration.Hspec (specForAppNegative)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)

-- A tiny WAI app that always returns 400 for any request.
app :: Application
app _req respond = respond $ responseLBS status400 [] "bad request"

main :: IO ()
main = do
    specResult <- loadOpenApiFile "examples/openapi.yaml"
    case specResult of
        Left err -> error (show err)
        Right openApi -> do
            let config = defaultTestConfig{tcNegativeTesting = True}
            hspec (specForAppNegative config (openApi :: OpenApi) app)
