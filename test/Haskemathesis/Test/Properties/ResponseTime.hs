{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for max response time check.

These tests verify that the 'maxResponseTime' check correctly:

1. Passes when response time is within the limit
2. Fails when response time exceeds the limit
3. Handles edge cases (exact limit, zero time)
-}
module Haskemathesis.Test.Properties.ResponseTime (spec) where

import Hedgehog (Property, assert, failure, forAll, property, success)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Standard.ResponseTime (maxResponseTime)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Max response time check" $ do
        itProp "passes when time is under limit" propPassesUnderLimit
        itProp "fails when time exceeds limit" propFailsOverLimit
        itProp "passes when time equals limit exactly" propPassesAtExactLimit
        itProp "passes for zero response time" propPassesZeroTime
        itProp "check name includes threshold" propCheckNameIncludesThreshold

-- | Response times under the limit should pass
propPassesUnderLimit :: Property
propPassesUnderLimit = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 100 10000)
    -- Generate a time that's definitely under the limit (at most 90%)
    actualMs <- forAll $ Gen.int (Range.linear 0 (maxMs * 9 `div` 10))
    let check = maxResponseTime maxMs
        res = makeResponse actualMs
        op = emptyOperation
    case checkRun check (dummyRequest op) res op of
        CheckPassed -> success
        CheckFailed _ -> failure

-- | Response times over the limit should fail
propFailsOverLimit :: Property
propFailsOverLimit = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 100 10000)
    -- Generate a time that's definitely over the limit (at least 110%)
    let overMs = maxMs + max 1 (maxMs `div` 10)
    actualMs <- forAll $ Gen.int (Range.linear overMs (overMs + 1000))
    let check = maxResponseTime maxMs
        res = makeResponse actualMs
        op = emptyOperation
    case checkRun check (dummyRequest op) res op of
        CheckFailed _ -> success
        CheckPassed -> failure

-- | Response time exactly at the limit should pass
propPassesAtExactLimit :: Property
propPassesAtExactLimit = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 1 10000)
    let check = maxResponseTime maxMs
        res = makeResponse maxMs
        op = emptyOperation
    case checkRun check (dummyRequest op) res op of
        CheckPassed -> success
        CheckFailed _ -> failure

-- | Zero response time should always pass
propPassesZeroTime :: Property
propPassesZeroTime = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 1 10000)
    let check = maxResponseTime maxMs
        res = makeResponse 0
        op = emptyOperation
    case checkRun check (dummyRequest op) res op of
        CheckPassed -> success
        CheckFailed _ -> failure

-- | Check name should include the threshold value
propCheckNameIncludesThreshold :: Property
propCheckNameIncludesThreshold = property $ do
    maxMs <- forAll $ Gen.int (Range.linear 1 10000)
    let check = maxResponseTime maxMs
        name = checkName check
    -- Name should contain the threshold value
    assert $ show maxMs `isInfixOf` show name
  where
    isInfixOf :: (Eq a) => [a] -> [a] -> Bool
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)

    isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

-- | Helper to create a response with a specific time in milliseconds
makeResponse :: Int -> ApiResponse
makeResponse ms =
    ApiResponse
        { resStatusCode = 200
        , resHeaders = []
        , resBody = ""
        , resTime = fromIntegral ms / 1000 -- Convert ms to seconds (NominalDiffTime)
        }
