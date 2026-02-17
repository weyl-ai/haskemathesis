{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Config (spec) where

import Hedgehog (Property, assert, property)
import Test.Hspec (Spec, describe)

import Haskemathesis.Config (filterByOperationId, filterByPathPrefix, filterByTag)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "Operation filters" $ do
        itProp "filter by tag" prop_filter_by_tag
        itProp "filter by operation id" prop_filter_by_operation_id
        itProp "filter by path prefix" prop_filter_by_path_prefix
        itProp "composed filters" prop_filter_composed

prop_filter_by_tag :: Property
prop_filter_by_tag =
    property $ do
        let op = emptyOperation{roTags = ["public", "beta"]}
        assert (filterByTag "public" op)
        assert (not (filterByTag "admin" op))

prop_filter_by_operation_id :: Property
prop_filter_by_operation_id =
    property $ do
        let op = emptyOperation{roOperationId = Just "getItems"}
        assert (filterByOperationId "getItems" op)
        assert (not (filterByOperationId "other" op))

prop_filter_by_path_prefix :: Property
prop_filter_by_path_prefix =
    property $ do
        let op = emptyOperation{roPath = "/api/v1/items"}
        assert (filterByPathPrefix "/api/v1" op)
        assert (not (filterByPathPrefix "/admin" op))

prop_filter_composed :: Property
prop_filter_composed =
    property $ do
        let op = emptyOperation{roTags = ["public"], roPath = "/api/v1/items"}
            composed o = filterByTag "public" o && filterByPathPrefix "/api/v1" o
        assert (composed op)
        assert (not (composed op{roTags = ["admin"]}))
        assert (not (composed op{roPath = "/admin"}))
