{-# LANGUAGE OverloadedStrings #-}

{- | Internal utilities for the Haskemathesis CLI.

This module provides shared utilities used by both the CLI and Runner modules.
-}
module Haskemathesis.CLI.Internal (
    matchesPattern,
    filterByAnyTag,
    buildOperationFilter,
    buildTestConfig,
    buildHeaders,
    buildChecks,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (HeaderName)

import Haskemathesis.CLI.Options (TestOptions (..))
import Haskemathesis.Check.Standard (allChecks, maxResponseTime)
import Haskemathesis.Check.Types (Check)
import Haskemathesis.Config (TestConfig (..), defaultStatefulChecks)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

-- | Check if an operation matches a pattern.
matchesPattern :: ResolvedOperation -> Text -> Bool
matchesPattern op pat =
    case roOperationId op of
        Just opId -> opId == pat
        Nothing -> roMethod op <> " " <> roPath op == pat

-- | Filter by any of the given tags.
filterByAnyTag :: [Text] -> ResolvedOperation -> Bool
filterByAnyTag tags op = any (\t -> t `elem` roTags op) tags

-- | Build operation filter from CLI options.
buildOperationFilter :: TestOptions -> (ResolvedOperation -> Bool)
buildOperationFilter opts =
    let includeFilter = case testInclude opts of
            [] -> const True
            patterns -> \op -> any (matchesPattern op) patterns
        excludeFilter = case testExclude opts of
            [] -> const True
            patterns -> \op -> not (any (matchesPattern op) patterns)
        tagFilter = case testTags opts of
            [] -> const True
            tags -> filterByAnyTag tags
     in \op -> includeFilter op && excludeFilter op && tagFilter op

-- | Build test config from CLI options.
buildTestConfig :: TestOptions -> TestConfig
buildTestConfig opts =
    TestConfig
        { tcChecks = buildChecks opts
        , tcAuthConfig = Nothing
        , tcBaseUrl = Just (testBaseUrl opts)
        , tcPropertyCount = testCount opts
        , tcNegativeTesting = testNegative opts
        , tcOperationFilter = const True
        , tcHeaders = buildHeaders opts
        , tcStreamingTimeout = testStreamingTimeout opts
        , tcStatefulTesting = testStateful opts
        , tcStatefulChecks = defaultStatefulChecks
        , tcMaxSequenceLength = testMaxSequenceLength opts
        , tcCleanupOnFailure = True
        }

-- | Build checks list from CLI options.
buildChecks :: TestOptions -> [Check]
buildChecks opts =
    allChecks ++ responseTimeCheck
  where
    responseTimeCheck = case testMaxResponseTime opts of
        Just maxMs -> [maxResponseTime maxMs]
        Nothing -> []

-- | Build global headers from CLI options.
buildHeaders :: TestOptions -> [(HeaderName, BS.ByteString)]
buildHeaders opts =
    case testAuthHeader opts of
        Just header -> [("Authorization", encodeUtf8 header)]
        Nothing -> []
