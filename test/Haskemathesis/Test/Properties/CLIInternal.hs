{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for CLI internal helpers.

These tests verify that CLI helper functions correctly:

1. Match operations by operationId or method+path
2. Filter operations by tags
3. Build operation filters from options
4. Build test configs from options
-}
module Haskemathesis.Test.Properties.CLIInternal (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.CLI.Internal (buildChecks, buildHeaders, buildOperationFilter, filterByAnyTag, matchesPattern)
import Haskemathesis.CLI.Options (TestOptions (..), defaultTestOptions)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "CLI Internal Helpers" $ do
        describe "matchesPattern" $ do
            itProp "matches by operationId when present" propMatchesByOperationId
            itProp "falls back to method+path when no operationId" propMatchesByMethodPath
            itProp "does not match different operationId" propNoMatchDifferentOpId
            itProp "does not match different method+path" propNoMatchDifferentMethodPath

        describe "filterByAnyTag" $ do
            itProp "returns true if any tag matches" propTagMatchesAny
            itProp "returns false if no tags match" propTagMatchesNone
            itProp "handles empty tag list" propTagEmptyList

        describe "buildOperationFilter" $ do
            itProp "includes all when no filters" propFilterAllWhenEmpty
            itProp "respects include patterns" propFilterRespectsInclude
            itProp "respects exclude patterns" propFilterRespectsExclude
            itProp "respects tag filters" propFilterRespectsTags

        describe "buildChecks" $ do
            itProp "includes maxResponseTime when set" propChecksIncludesResponseTime
            itProp "does not include maxResponseTime when not set" propChecksExcludesResponseTime

        describe "buildHeaders" $ do
            itProp "includes Authorization when authHeader set" propHeadersIncludesAuth
            itProp "returns empty when no authHeader" propHeadersEmptyWhenNoAuth

-- Generators

genOperationId :: Gen Text
genOperationId = Gen.text (Range.linear 3 20) Gen.alphaNum

genMethod :: Gen Text
genMethod = Gen.element ["GET", "POST", "PUT", "DELETE", "PATCH"]

genPath :: Gen Text
genPath = do
    segments <- Gen.list (Range.linear 1 4) (Gen.text (Range.linear 1 10) Gen.alphaNum)
    pure $ "/" <> T.intercalate "/" segments

genTag :: Gen Text
genTag = Gen.text (Range.linear 2 15) Gen.alpha

-- matchesPattern properties

propMatchesByOperationId :: Property
propMatchesByOperationId = property $ do
    opId <- forAll genOperationId
    method <- forAll genMethod
    path <- forAll genPath
    let op = emptyOperation{roOperationId = Just opId, roMethod = method, roPath = path}
    assert $ matchesPattern op opId

propMatchesByMethodPath :: Property
propMatchesByMethodPath = property $ do
    method <- forAll genMethod
    path <- forAll genPath
    let op = emptyOperation{roOperationId = Nothing, roMethod = method, roPath = path}
        pattern' = method <> " " <> path
    assert $ matchesPattern op pattern'

propNoMatchDifferentOpId :: Property
propNoMatchDifferentOpId = property $ do
    opId <- forAll genOperationId
    differentOpId <- forAll $ Gen.filter (/= opId) genOperationId
    let op = emptyOperation{roOperationId = Just opId}
    assert $ not (matchesPattern op differentOpId)

propNoMatchDifferentMethodPath :: Property
propNoMatchDifferentMethodPath = property $ do
    method <- forAll genMethod
    path <- forAll genPath
    differentPath <- forAll $ Gen.filter (/= path) genPath
    let op = emptyOperation{roOperationId = Nothing, roMethod = method, roPath = path}
        pattern' = method <> " " <> differentPath
    assert $ not (matchesPattern op pattern')

-- filterByAnyTag properties

propTagMatchesAny :: Property
propTagMatchesAny = property $ do
    opTags <- forAll $ Gen.list (Range.linear 1 5) genTag
    filterTags <- forAll $ Gen.list (Range.linear 1 3) genTag
    -- Ensure at least one filter tag is in op tags
    matchingTag <- forAll $ Gen.element filterTags
    let op = emptyOperation{roTags = matchingTag : opTags}
    assert $ filterByAnyTag filterTags op

propTagMatchesNone :: Property
propTagMatchesNone = property $ do
    opTags <- forAll $ Gen.list (Range.linear 1 5) genTag
    -- Create filter tags that definitely don't overlap
    let filterTags = map ("no_" <>) opTags
    let op = emptyOperation{roTags = opTags}
    assert $ not (filterByAnyTag filterTags op)

propTagEmptyList :: Property
propTagEmptyList = property $ do
    opTags <- forAll $ Gen.list (Range.linear 0 5) genTag
    let op = emptyOperation{roTags = opTags}
    -- Empty filter list should return False (no tags to match)
    assert $ not (filterByAnyTag [] op)

-- buildOperationFilter properties

propFilterAllWhenEmpty :: Property
propFilterAllWhenEmpty = property $ do
    method <- forAll genMethod
    path <- forAll genPath
    let opts = defaultTestOptions
        op = emptyOperation{roMethod = method, roPath = path}
        filterFn = buildOperationFilter opts
    assert $ filterFn op

propFilterRespectsInclude :: Property
propFilterRespectsInclude = property $ do
    opId <- forAll genOperationId
    let op = emptyOperation{roOperationId = Just opId}
        opts = defaultTestOptions{testInclude = [opId]}
        filterFn = buildOperationFilter opts
    assert $ filterFn op

propFilterRespectsExclude :: Property
propFilterRespectsExclude = property $ do
    opId <- forAll genOperationId
    let op = emptyOperation{roOperationId = Just opId}
        opts = defaultTestOptions{testExclude = [opId]}
        filterFn = buildOperationFilter opts
    assert $ not (filterFn op)

propFilterRespectsTags :: Property
propFilterRespectsTags = property $ do
    tag <- forAll genTag
    let op = emptyOperation{roTags = [tag]}
        opts = defaultTestOptions{testTags = [tag]}
        filterFn = buildOperationFilter opts
    assert $ filterFn op

-- buildChecks properties

propChecksIncludesResponseTime :: Property
propChecksIncludesResponseTime = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 100 10000)
    let opts = defaultTestOptions{testMaxResponseTime = Just maxMs}
        checks = buildChecks opts
    -- Check that we have more checks than default (allChecks)
    -- The response time check should be added
    assert $ length checks > 5 -- allChecks has 5 checks

propChecksExcludesResponseTime :: Property
propChecksExcludesResponseTime = property $ do
    let opts = defaultTestOptions{testMaxResponseTime = Nothing}
        checks = buildChecks opts
    -- Should have exactly allChecks (5 checks)
    assert $ length checks == 5

-- buildHeaders properties

propHeadersIncludesAuth :: Property
propHeadersIncludesAuth = property $ do
    authValue <- forAll $ Gen.text (Range.linear 5 50) Gen.alphaNum
    let opts = defaultTestOptions{testAuthHeader = Just ("Bearer " <> authValue)}
        headers = buildHeaders opts
    assert $ not (null headers)
    case headers of
        ((name, _) : _) -> assert $ name == "Authorization"
        [] -> assert False

propHeadersEmptyWhenNoAuth :: Property
propHeadersEmptyWhenNoAuth = property $ do
    let opts = defaultTestOptions{testAuthHeader = Nothing}
        headers = buildHeaders opts
    assert $ null headers
