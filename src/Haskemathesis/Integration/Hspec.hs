{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Integration.Hspec (
    specForExecutor,
    specForApp,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (check)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.OpenApi (OpenApi)
import Network.Wai (Application)

import Haskemathesis.Check.Types (Check)
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse, BaseUrl)
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertyForOperation)

specForExecutor ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    Spec
specForExecutor mBase checks execute ops =
    describe "OpenAPI Conformance" $
        mapM_ (specForOperation mBase checks execute) ops

specForApp ::
    Maybe BaseUrl ->
    [Check] ->
    OpenApi ->
    Application ->
    Spec
specForApp mBase checks openApi app =
    specForExecutor mBase checks (executeWai app) (resolveOperations openApi)

specForOperation ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Spec
specForOperation mBase checks execute op =
    it (T.unpack (operationLabel op)) $
        check (propertyForOperation mBase checks execute op) >>= (`shouldBe` True)

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
