{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Check.Standard.ResponseTime
Description : Response time conformance check
Stability   : experimental

This module provides a check for validating that API response times
stay within acceptable limits. This is useful for performance testing
and SLA compliance verification.
-}
module Haskemathesis.Check.Standard.ResponseTime (
    maxResponseTime,
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Haskemathesis.Check.Standard.Helpers (failureDetail)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))

{- | Create a check that fails if response time exceeds the given limit.

This check validates that API responses are returned within an acceptable
time frame. It's useful for:

* Performance regression testing
* SLA compliance verification
* Identifying slow endpoints

==== Parameters

* @maxMs@ - Maximum allowed response time in milliseconds

==== Behavior

* __Passes__ when response time is less than or equal to @maxMs@
* __Fails__ when response time exceeds @maxMs@

==== Example

@
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Check.Standard.ResponseTime (maxResponseTime)

-- Fail if any response takes longer than 500ms
checks = defaultChecks ++ [maxResponseTime 500]

-- Strict SLA: fail if response exceeds 100ms
strictChecks = defaultChecks ++ [maxResponseTime 100]
@

==== Notes

* Response time is measured from request start to response completion
* For streaming endpoints, this measures time until the configured
  timeout or stream end, whichever comes first
* The check name includes the threshold for clarity in reports:
  e.g., "max_response_time_500ms"
-}
maxResponseTime :: Int -> Check
maxResponseTime maxMs =
    Check name $ \req res op ->
        let actualMs = nominalDiffTimeToMs (resTime res)
            -- Use small epsilon for floating point comparison
            limit = fromIntegral maxMs + 0.001
         in if actualMs <= limit
                then CheckPassed
                else
                    CheckFailed
                        ( failureDetail
                            name
                            (formatMessage actualMs maxMs)
                            []
                            Nothing
                            req
                            res
                            op
                        )
  where
    name :: Text
    name = "max_response_time_" <> T.pack (show maxMs) <> "ms"

-- | Convert NominalDiffTime to milliseconds as a Double
nominalDiffTimeToMs :: NominalDiffTime -> Double
nominalDiffTimeToMs dt = realToFrac dt * 1000

-- | Format the failure message with actual and expected times
formatMessage :: Double -> Int -> Text
formatMessage actualMs maxMs =
    "response time "
        <> T.pack (showMs actualMs)
        <> " exceeds limit of "
        <> T.pack (show maxMs)
        <> "ms"

-- | Show milliseconds with reasonable precision
showMs :: Double -> String
showMs ms
    | ms < 1 = show (round (ms * 1000) :: Int) <> "us"
    | ms < 1000 = show (round ms :: Int) <> "ms"
    | otherwise = show (fromIntegral (round (ms * 10) :: Int) / 10 :: Double) <> "ms"
