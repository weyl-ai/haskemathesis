{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseLBS)
import Test.Hspec (hspec)

import Haskemathesis.Check.Standard (allChecks)
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.Integration.Hspec (specForExecutor)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)

main :: IO ()
main = do
    specResult <- loadOpenApiFile "examples/openapi.yaml"
    case specResult of
        Left err -> error (show err)
        Right spec ->
            hspec (specForExecutor Nothing allChecks (executeWai app) (resolveOperations spec))

app :: Application
app _request respond =
    respond
        ( responseLBS
            status200
            [(hContentType, "application/json"), ("X-Request-Id", "req-1")]
            (LBS.fromStrict "\"pong\"")
        )
