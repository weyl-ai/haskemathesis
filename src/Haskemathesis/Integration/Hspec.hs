{-# LANGUAGE OverloadedStrings #-}

-- | Hspec integration helpers for generated properties.
module Haskemathesis.Integration.Hspec (
    specForExecutor,
    specForApp,
    specForExecutorWithConfig,
    specForAppWithConfig,
    specForUrl,
    specForExecutorNegative,
    specForAppNegative,
    specForUrlNegative,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (check)
import Test.Hspec (Spec, describe, it, shouldBe)

import Data.OpenApi (OpenApi)
import Network.HTTP.Client (Manager)
import Network.Wai (Application)

import Haskemathesis.Check.Types (Check)
import Haskemathesis.Config (TestConfig (..))
import Haskemathesis.Execute.Http (executeHttp)
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse, BaseUrl)
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertyForOperation, propertyForOperationWithConfig)

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

specForExecutorWithConfig ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    Spec
specForExecutorWithConfig openApi config execute ops =
    describe "OpenAPI Conformance" $
        mapM_ (specForOperationWithConfig openApi config execute) ops

specForAppWithConfig ::
    TestConfig ->
    OpenApi ->
    Application ->
    Spec
specForAppWithConfig config openApi app =
    specForExecutorWithConfig openApi config (executeWai app) (resolveOperations openApi)

specForUrl ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    Spec
specForUrl config openApi manager baseUrl =
    let config' = config{tcBaseUrl = Just baseUrl}
     in specForExecutorWithConfig openApi config' (executeHttp manager baseUrl) (resolveOperations openApi)

specForExecutorNegative ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    Spec
specForExecutorNegative openApi config execute ops =
    let config' = config{tcNegativeTesting = True}
     in specForExecutorWithConfig openApi config' execute ops

specForAppNegative ::
    TestConfig ->
    OpenApi ->
    Application ->
    Spec
specForAppNegative config openApi app =
    specForExecutorNegative openApi config (executeWai app) (resolveOperations openApi)

specForUrlNegative ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    Spec
specForUrlNegative config openApi manager baseUrl =
    specForExecutorNegative openApi (config{tcBaseUrl = Just baseUrl}) (executeHttp manager baseUrl) (resolveOperations openApi)

specForOperation ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Spec
specForOperation mBase checks execute op =
    it (T.unpack (operationLabel op)) $
        check (propertyForOperation mBase checks execute op) >>= (`shouldBe` True)

specForOperationWithConfig ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Spec
specForOperationWithConfig openApi config execute op
    | not (tcOperationFilter config op) = pure ()
    | otherwise =
        it (T.unpack (operationLabel op)) $
            check (propertyForOperationWithConfig openApi config execute op) >>= (`shouldBe` True)

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
