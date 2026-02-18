{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Config (spec) where

import Haskemathesis.Config (filterByOperationId, filterByPathPrefix, filterByTag)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Test.Support (emptyOperation, itProp)
import Hedgehog (Property, assert, property)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
    describe "Operation filters" $ do
        itProp "filter by tag" prop_filter_by_tag
        itProp "filter by operation id" prop_filter_by_operation_id
        itProp "filter by path prefix" prop_filter_by_path_prefix
        itProp "composed filters" prop_filter_composed
        -- Additional config tests
        itProp "filter rejects non-matching paths" prop_filter_rejects_non_matching
        itProp "empty tag list not matched" prop_empty_tag_not_matched
        itProp "negated filter works" prop_negated_filter

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

-- | Filter should reject operations that don't match
prop_filter_rejects_non_matching :: Property
prop_filter_rejects_non_matching =
    property $ do
        let op1 = emptyOperation{roPath = "/users/1"}
            op2 = emptyOperation{roPath = "/items/2"}
            op3 = emptyOperation{roPath = "/admin/settings"}
            userFilter = filterByPathPrefix "/users"
        assert (userFilter op1)
        assert (not (userFilter op2))
        assert (not (userFilter op3))

-- | Operation with no tags should not match any tag filter
prop_empty_tag_not_matched :: Property
prop_empty_tag_not_matched =
    property $ do
        let op = emptyOperation{roTags = []}
        assert (not (filterByTag "any" op))
        assert (not (filterByTag "" op))

-- | Negated filters should work correctly
prop_negated_filter :: Property
prop_negated_filter =
    property $ do
        let op1 = emptyOperation{roTags = ["internal"]}
            op2 = emptyOperation{roTags = ["public"]}
            notInternal o = not (filterByTag "internal" o)
        assert (not (notInternal op1))
        assert (notInternal op2)
