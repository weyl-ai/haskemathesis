{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Report.JUnit
Description : JUnit XML report generation
Stability   : experimental

This module provides functions for generating JUnit XML reports from
test results. JUnit XML is a widely-supported format that CI systems
(Jenkins, GitLab CI, GitHub Actions, CircleCI, etc.) can parse to
display test results.

=== Basic Usage

@
import Haskemathesis.Report.JUnit (renderJUnitXml, TestSuiteResult(..))

let results = [TestCaseResult "get_users" True 0.05 Nothing, ...]
    xml = renderJUnitXml "haskemathesis" results
writeFile "test-results.xml" (T.unpack xml)
@

=== CI Integration

Most CI systems automatically detect JUnit XML files. For example:

* __GitHub Actions__: Use @actions/upload-artifact@ and test reporters
* __GitLab CI__: Set @artifacts:reports:junit@ in @.gitlab-ci.yml@
* __Jenkins__: Use the JUnit plugin
* __CircleCI__: Use the @store_test_results@ step
-}
module Haskemathesis.Report.JUnit (
    -- * Test Result Types
    TestCaseResult (..),
    TestSuiteResult (..),

    -- * Rendering
    renderJUnitXml,
    renderTestSuite,
)
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)

{- | Result of a single test case (one operation).

Each test case represents one API operation being tested.
-}
data TestCaseResult = TestCaseResult
    { tcrName :: !Text
    -- ^ Name of the test case (typically the operation ID or method+path)
    , tcrPassed :: !Bool
    -- ^ Whether the test passed
    , tcrTime :: !NominalDiffTime
    -- ^ Time taken for the test in seconds
    , tcrFailureMessage :: !(Maybe Text)
    -- ^ Failure message if the test failed
    , tcrTestCount :: !Int
    -- ^ Number of individual property tests run
    }
    deriving (Eq, Show)

{- | Result of a test suite (all operations).

A test suite contains multiple test cases.
-}
data TestSuiteResult = TestSuiteResult
    { tsrName :: !Text
    -- ^ Name of the test suite (e.g., "haskemathesis" or the spec name)
    , tsrTests :: ![TestCaseResult]
    -- ^ List of test case results
    , tsrTime :: !NominalDiffTime
    -- ^ Total time taken for all tests
    }
    deriving (Eq, Show)

{- | Render test results as JUnit XML.

This produces a valid JUnit XML document that can be parsed by CI systems.

=== Parameters

* @suiteName@ - Name for the test suite
* @results@ - List of test case results

=== Return Value

Returns JUnit XML as 'Text'.

=== Example Output

@
\<?xml version="1.0" encoding="UTF-8"?\>
\<testsuites tests="3" failures="1" time="0.5"\>
  \<testsuite name="haskemathesis" tests="3" failures="1" time="0.5"\>
    \<testcase name="GET /users" time="0.1"\>\</testcase\>
    \<testcase name="POST /users" time="0.2"\>
      \<failure message="Schema validation failed"\>Response body does not match schema\</failure\>
    \</testcase\>
    \<testcase name="GET /users/{id}" time="0.2"\>\</testcase\>
  \</testsuite\>
\</testsuites\>
@
-}
renderJUnitXml :: Text -> [TestCaseResult] -> Text
renderJUnitXml suiteName results =
    let totalTests = foldl' (\n _ -> n + 1) (0 :: Int) results
        failures = foldl' (\n r -> if tcrPassed r then n else n + 1) (0 :: Int) results
        totalTime = foldl' (+) 0 (map tcrTime results)
     in T.intercalate
            "\n"
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<testsuites tests=\""
                <> T.pack (show totalTests)
                <> "\" failures=\""
                <> T.pack (show failures)
                <> "\" time=\""
                <> formatTime totalTime
                <> "\">"
            , renderTestSuite (TestSuiteResult suiteName results totalTime)
            , "</testsuites>"
            ]

{- | Render a single test suite as XML.

This is useful if you need to generate multiple test suites in one document.
-}
renderTestSuite :: TestSuiteResult -> Text
renderTestSuite suite =
    let totalTests = foldl' (\n _ -> n + 1) (0 :: Int) (tsrTests suite)
        failures = foldl' (\n r -> if tcrPassed r then n else n + 1) (0 :: Int) (tsrTests suite)
     in T.intercalate
            "\n"
            ( [ "  <testsuite name=\""
                    <> escapeXml (tsrName suite)
                    <> "\" tests=\""
                    <> T.pack (show totalTests)
                    <> "\" failures=\""
                    <> T.pack (show failures)
                    <> "\" time=\""
                    <> formatTime (tsrTime suite)
                    <> "\">"
              ]
                ++ map renderTestCase (tsrTests suite)
                ++ ["  </testsuite>"]
            )

-- | Render a single test case as XML.
renderTestCase :: TestCaseResult -> Text
renderTestCase tc
    | tcrPassed tc =
        "    <testcase name=\""
            <> escapeXml (tcrName tc)
            <> "\" time=\""
            <> formatTime (tcrTime tc)
            <> "\"></testcase>"
    | otherwise =
        T.intercalate
            "\n"
            [ "    <testcase name=\""
                <> escapeXml (tcrName tc)
                <> "\" time=\""
                <> formatTime (tcrTime tc)
                <> "\">"
            , "      <failure message=\""
                <> escapeXml (fromMaybe "Test failed" (tcrFailureMessage tc))
                <> "\">"
                <> escapeXml (fromMaybe "" (tcrFailureMessage tc))
                <> "</failure>"
            , "    </testcase>"
            ]

-- | Format time as seconds with 3 decimal places.
formatTime :: NominalDiffTime -> Text
formatTime dt =
    let seconds = realToFrac dt :: Double
        -- Show with 3 decimal places
        formatted = show (fromIntegral (round (seconds * 1000) :: Int) / 1000 :: Double)
     in T.pack formatted

{- | Escape special XML characters.
Important: & must be replaced FIRST to avoid double-escaping entities.
-}
escapeXml :: Text -> Text
escapeXml =
    T.replace "'" "&apos;"
        . T.replace "\"" "&quot;"
        . T.replace ">" "&gt;"
        . T.replace "<" "&lt;"
        . T.replace "&" "&amp;"
