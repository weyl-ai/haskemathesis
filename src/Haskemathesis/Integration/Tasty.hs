{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Integration.Tasty (
    testTreeForExecutor,
    testTreeForApp,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog.Internal.Property (PropertyName (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.OpenApi (OpenApi)
import Network.Wai (Application)

import Haskemathesis.Check.Types (Check)
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse, BaseUrl)
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertyForOperation)

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

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
