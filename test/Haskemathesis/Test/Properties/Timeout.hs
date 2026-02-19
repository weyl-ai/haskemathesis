{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for effectiveTimeout function.

These tests verify that effectiveTimeout correctly:

1. Uses x-timeout when explicitly set
2. Uses config streaming timeout for streaming operations
3. Returns Nothing for non-streaming operations without x-timeout
4. Prefers x-timeout over streaming timeout
-}
module Haskemathesis.Test.Properties.Timeout (spec) where

import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (effectiveTimeout)
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "effectiveTimeout" $ do
        itProp "uses x-timeout when set" propUsesExplicitTimeout
        itProp "uses config timeout for streaming operations" propUsesConfigForStreaming
        itProp "returns Nothing for non-streaming without x-timeout" propNothingForNonStreaming
        itProp "prefers x-timeout over streaming default" propPrefersExplicitOverStreaming
        itProp "returns Nothing when streaming but no config timeout" propNothingWhenNoConfigTimeout

-- Properties

propUsesExplicitTimeout :: Property
propUsesExplicitTimeout = property $ do
    timeout <- forAll $ Gen.int (Range.linear 100 10000)
    let op = emptyOperation{roTimeout = Just timeout, roIsStreaming = False}
        config = defaultTestConfig
    effectiveTimeout config op === Just timeout

propUsesConfigForStreaming :: Property
propUsesConfigForStreaming = property $ do
    configTimeout <- forAll $ Gen.int (Range.linear 100 10000)
    let op = emptyOperation{roTimeout = Nothing, roIsStreaming = True}
        config = defaultTestConfig{tcStreamingTimeout = Just configTimeout}
    effectiveTimeout config op === Just configTimeout

propNothingForNonStreaming :: Property
propNothingForNonStreaming = property $ do
    let op = emptyOperation{roTimeout = Nothing, roIsStreaming = False}
        config = defaultTestConfig
    effectiveTimeout config op === Nothing

propPrefersExplicitOverStreaming :: Property
propPrefersExplicitOverStreaming = property $ do
    explicitTimeout <- forAll $ Gen.int (Range.linear 100 5000)
    configTimeout <- forAll $ Gen.int (Range.linear 5001 10000)
    let op = emptyOperation{roTimeout = Just explicitTimeout, roIsStreaming = True}
        config = defaultTestConfig{tcStreamingTimeout = Just configTimeout}
    -- x-timeout should take precedence even for streaming operations
    effectiveTimeout config op === Just explicitTimeout

propNothingWhenNoConfigTimeout :: Property
propNothingWhenNoConfigTimeout = property $ do
    let op = emptyOperation{roTimeout = Nothing, roIsStreaming = True}
        config = defaultTestConfig{tcStreamingTimeout = Nothing}
    effectiveTimeout config op === Nothing
