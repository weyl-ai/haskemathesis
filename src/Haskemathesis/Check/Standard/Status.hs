{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Check.Standard.Status
Description : HTTP status code conformance checks
Stability   : experimental

This module provides checks for validating HTTP response status codes
against OpenAPI specifications. It includes checks for server errors
and status code documentation conformance.
-}
module Haskemathesis.Check.Standard.Status (
    notAServerError,
    statusCodeConformance,
)
where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T
import Haskemathesis.Check.Standard.Helpers (failureDetail)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

{- | Check that the API does not return a server error (5xx status code).

This is a fundamental availability check that ensures the server is
functioning correctly. Any 5xx response indicates a server-side failure
that should be investigated.

==== Behavior

* __Passes__ when the response status code is less than 500
* __Fails__ when the response status code is 500 or higher

==== Example

@
-- Include in your standard checks
checks = ['notAServerError', 'statusCodeConformance']
@
-}
notAServerError :: Check
notAServerError =
    Check "not_a_server_error" $ \req res op ->
        if resStatusCode res >= 500
            then CheckFailed (failureDetail "not_a_server_error" "response status is 5xx" [] Nothing req res op)
            else CheckPassed

{- | Check that the response status code is documented in the OpenAPI spec.

This check validates that every status code returned by the API is either:

* Explicitly documented in the operation's @responses@ object
* Covered by a @default@ response specification

Undocumented status codes indicate incomplete API documentation or
unexpected API behavior.

==== Behavior

* __Passes__ when the status code is documented or a default response exists
* __Fails__ when the status code is not documented and no default exists

==== Example

@
-- Ensure all responses match documentation
checks = ['notAServerError', 'statusCodeConformance']
@
-}
statusCodeConformance :: Check
statusCodeConformance =
    Check "status_code_conformance" $ \req res op ->
        if statusAllowed (resStatusCode res) op
            then CheckPassed
            else
                CheckFailed
                    ( failureDetail
                        "status_code_conformance"
                        ("response status code is not documented: " <> T.pack (show (resStatusCode res)))
                        []
                        Nothing
                        req
                        res
                        op
                    )

statusAllowed :: Int -> ResolvedOperation -> Bool
statusAllowed status op =
    Map.member status (roResponses op) || isJust (roDefaultResponse op)
