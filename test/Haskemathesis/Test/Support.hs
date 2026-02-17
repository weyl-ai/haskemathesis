{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Support (
    itProp,
    emptyOperation,
    dummyRequest,
) where

import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, check)
import Test.Hspec (Spec, it, shouldBe)

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

itProp :: String -> Property -> Spec
itProp label prop =
    it label $ do
        ok <- check prop
        ok `shouldBe` True

emptyOperation :: ResolvedOperation
emptyOperation =
    ResolvedOperation
        { roMethod = "GET"
        , roPath = "/"
        , roOperationId = Nothing
        , roParameters = []
        , roRequestBody = Nothing
        , roResponses = mempty
        , roDefaultResponse = Nothing
        }

dummyRequest :: ResolvedOperation -> ApiRequest
dummyRequest op =
    ApiRequest
        { reqMethod = encodeUtf8 (roMethod op)
        , reqPath = roPath op
        , reqQueryParams = []
        , reqHeaders = []
        , reqBody = Nothing
        }
