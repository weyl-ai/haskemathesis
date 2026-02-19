{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for JUnit XML report generation.

These tests verify that the JUnit XML output is valid and correctly
represents test results.
-}
module Haskemathesis.Test.Properties.JUnit (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen, Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Report.JUnit (TestCaseResult (..), renderJUnitXml)
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "JUnit XML output" $ do
        itProp "produces valid XML structure" propValidXmlStructure
        itProp "includes correct test count" propCorrectTestCount
        itProp "includes correct failure count" propCorrectFailureCount
        itProp "escapes special XML characters" propEscapesXmlChars
        itProp "handles empty test list" propHandlesEmptyList

-- | Output should have valid XML structure
propValidXmlStructure :: Property
propValidXmlStructure = property $ do
    results <- forAll genTestResults
    let xml = renderJUnitXml "test-suite" results
    -- Check for required XML elements
    assert $ "<?xml version=\"1.0\"" `T.isInfixOf` xml
    assert $ "<testsuites" `T.isInfixOf` xml
    assert $ "</testsuites>" `T.isInfixOf` xml
    assert $ "<testsuite" `T.isInfixOf` xml
    assert $ "</testsuite>" `T.isInfixOf` xml

-- | Test count in XML matches actual number of tests
propCorrectTestCount :: Property
propCorrectTestCount = property $ do
    results <- forAll genTestResults
    let count = foldl' (\n _ -> n + 1) (0 :: Int) results
    let xml = renderJUnitXml "test-suite" results
    -- Check that tests="N" appears in the output
    assert $ ("tests=\"" <> T.pack (show count) <> "\"") `T.isInfixOf` xml

-- | Failure count in XML matches actual number of failures
propCorrectFailureCount :: Property
propCorrectFailureCount = property $ do
    results <- forAll genTestResults
    let failures = foldl' (\n r -> if tcrPassed r then n else n + 1) (0 :: Int) results
    let xml = renderJUnitXml "test-suite" results
    -- Check that failures="N" appears in the output
    assert $ ("failures=\"" <> T.pack (show failures) <> "\"") `T.isInfixOf` xml

-- | Special XML characters should be escaped
propEscapesXmlChars :: Property
propEscapesXmlChars = property $ do
    -- Create a test with special characters in the name
    let specialName = "test<name>"
        result =
            TestCaseResult
                { tcrName = specialName
                , tcrPassed = True
                , tcrTime = 0.1
                , tcrFailureMessage = Nothing
                , tcrTestCount = 1
                }
    let xml = renderJUnitXml "test-suite" [result]
    -- The literal <name> should not appear (would break XML)
    assert $ not ("test<name>" `T.isInfixOf` xml)
    -- Escaped versions should appear instead
    assert $ "&lt;" `T.isInfixOf` xml
    assert $ "&gt;" `T.isInfixOf` xml

-- | Empty test list should still produce valid XML
propHandlesEmptyList :: Property
propHandlesEmptyList = property $ do
    let xml = renderJUnitXml "test-suite" []
    assert $ "<?xml version=\"1.0\"" `T.isInfixOf` xml
    assert $ "tests=\"0\"" `T.isInfixOf` xml
    assert $ "failures=\"0\"" `T.isInfixOf` xml

-- | Generate a list of test results
genTestResults :: Gen [TestCaseResult]
genTestResults = do
    count <- Gen.int (Range.linear 0 10)
    Gen.list (Range.singleton count) genTestCaseResult

-- | Generate a single test case result
genTestCaseResult :: Gen TestCaseResult
genTestCaseResult = do
    name <- genTestName
    passed <- Gen.bool
    time <- Gen.double (Range.linearFrac 0.001 5.0)
    let failMsg = if passed then Nothing else Just "Test failed"
    pure $
        TestCaseResult
            { tcrName = name
            , tcrPassed = passed
            , tcrTime = realToFrac time
            , tcrFailureMessage = failMsg
            , tcrTestCount = 1
            }

-- | Generate a test name
genTestName :: Gen Text
genTestName = do
    method <- Gen.element ["GET", "POST", "PUT", "DELETE", "PATCH"]
    path <- genPath
    pure $ method <> " " <> path

-- | Generate a path
genPath :: Gen Text
genPath = do
    segments <- Gen.int (Range.linear 1 4)
    parts <- Gen.list (Range.singleton segments) genPathSegment
    pure $ "/" <> T.intercalate "/" parts

-- | Generate a path segment
genPathSegment :: Gen Text
genPathSegment =
    Gen.element
        [ "users"
        , "posts"
        , "comments"
        , "items"
        , "{id}"
        , "{userId}"
        , "api"
        , "v1"
        , "v2"
        ]
