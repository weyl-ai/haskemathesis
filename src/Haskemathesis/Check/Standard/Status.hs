{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Standard.Status (
    notAServerError,
    statusCodeConformance,
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Text as T

import Haskemathesis.Check.Standard.Helpers (failureDetail)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

notAServerError :: Check
notAServerError =
    Check "not_a_server_error" $ \req res op ->
        if resStatusCode res >= 500
            then CheckFailed (failureDetail "not_a_server_error" "response status is 5xx" [] Nothing req res op)
            else CheckPassed

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
