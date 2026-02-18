{-# LANGUAGE StrictData #-}

{- | Core types for checks and failure reporting.

This module defines the fundamental types used for response validation
throughout Haskemathesis. Checks are the primary mechanism for asserting
that API responses conform to expectations.

=== Creating Custom Checks

You can define your own checks by creating a 'Check' value:

@
myCustomCheck :: Check
myCustomCheck = Check
    { checkName = "My Custom Check"
    , checkRun = \\req res op ->
        if resStatusCode res == 200
            then CheckPassed
            else CheckFailed $ FailureDetail
                { fdCheck = "My Custom Check"
                , fdMessage = "Expected 200, got " <> pack (show (resStatusCode res))
                , fdRequest = req
                , fdResponse = res
                , fdOperation = fromMaybe "unknown" (roOperationId op)
                , fdSchemaErrors = []
                , fdSchemaDiff = Nothing
                , fdMutation = Nothing
                }
    }
@
-}
module Haskemathesis.Check.Types (
    Check (..),
    CheckResult (..),
    FailureDetail (..),
)
where

import Data.Text (Text)
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse)
import Haskemathesis.OpenApi.Types (ResolvedOperation)

{- | A check validates that an API response meets a specific criterion.

Each check consists of a name and a function that examines the
request, response, and operation definition to determine if the
check passes or fails.
-}
data Check = Check
    { checkName :: Text
    -- ^ Human-readable name of the check, used in reports.
    , checkRun :: ApiRequest -> ApiResponse -> ResolvedOperation -> CheckResult
    {- ^ Function that performs the validation. Takes the request that was sent,
    the response that was received, and the operation definition.
    -}
    }

{- | The result of running a check.

A check either passes (indicating the response is valid according to
that criterion) or fails with detailed information about what went wrong.
-}
data CheckResult
    = CheckPassed
    | CheckFailed FailureDetail
    deriving (Eq, Show)

{- | Detailed information about a check failure.

This record contains all the context needed to diagnose and report
a test failure, including the request, response, and specific
error messages.
-}
data FailureDetail = FailureDetail
    { fdCheck :: Text
    -- ^ Name of the check that failed.
    , fdMessage :: Text
    -- ^ Human-readable description of what went wrong.
    , fdRequest :: ApiRequest
    -- ^ The request that was sent to the API.
    , fdResponse :: ApiResponse
    -- ^ The response received from the API.
    , fdOperation :: Text
    -- ^ Identifier for the operation being tested.
    , fdSchemaErrors :: [Text]
    -- ^ JSON Schema validation errors, if any.
    , fdSchemaDiff :: Maybe Text
    -- ^ Diff showing expected vs actual schema, if applicable.
    , fdMutation :: Maybe Text
    -- ^ Description of the mutation applied (for negative testing).
    }
    deriving (Eq, Show)
