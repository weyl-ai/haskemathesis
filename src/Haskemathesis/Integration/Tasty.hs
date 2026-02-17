{-# LANGUAGE OverloadedStrings #-}

-- | Tasty integration helpers for generated properties.
module Haskemathesis.Integration.Tasty (
    testTreeForExecutor,
    testTreeForApp,
    testTreeForExecutorWithConfig,
    testTreeForAppWithConfig,
    testTreeForUrl,
    testTreeForExecutorNegative,
    testTreeForAppNegative,
    testTreeForUrlNegative,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog.Internal.Property (PropertyName (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

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

testTreeForExecutor ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutor mBase checks execute ops =
    testGroup
        "OpenAPI Conformance"
        [ testPropertyNamed name (PropertyName name) (propertyForOperation mBase checks execute op)
        | op <- ops
        , let name = T.unpack (operationLabel op)
        ]

testTreeForApp ::
    Maybe BaseUrl ->
    [Check] ->
    OpenApi ->
    Application ->
    TestTree
testTreeForApp mBase checks openApi app =
    testTreeForExecutor mBase checks (executeWai app) (resolveOperations openApi)

testTreeForExecutorWithConfig ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutorWithConfig openApi config execute ops =
    testGroup
        "OpenAPI Conformance"
        [ testPropertyNamed name (PropertyName name) (propertyForOperationWithConfig openApi config execute op)
        | op <- ops
        , tcOperationFilter config op
        , let name = T.unpack (operationLabel op)
        ]

testTreeForAppWithConfig ::
    TestConfig ->
    OpenApi ->
    Application ->
    TestTree
testTreeForAppWithConfig config openApi app =
    testTreeForExecutorWithConfig openApi config (executeWai app) (resolveOperations openApi)

testTreeForUrl ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    TestTree
testTreeForUrl config openApi manager baseUrl =
    let config' = config{tcBaseUrl = Just baseUrl}
     in testTreeForExecutorWithConfig openApi config' (executeHttp manager baseUrl) (resolveOperations openApi)

testTreeForExecutorNegative ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutorNegative openApi config execute ops =
    let config' = config{tcNegativeTesting = True}
     in testTreeForExecutorWithConfig openApi config' execute ops

testTreeForAppNegative ::
    TestConfig ->
    OpenApi ->
    Application ->
    TestTree
testTreeForAppNegative config openApi app =
    testTreeForExecutorNegative openApi config (executeWai app) (resolveOperations openApi)

testTreeForUrlNegative ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    TestTree
testTreeForUrlNegative config openApi manager baseUrl =
    testTreeForExecutorNegative openApi (config{tcBaseUrl = Just baseUrl}) (executeHttp manager baseUrl) (resolveOperations openApi)

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
