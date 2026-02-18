{-# LANGUAGE OverloadedStrings #-}

{- | Checks that assert negative (invalid) requests are rejected.

This module provides checks for negative testing, which verifies that
the API properly rejects invalid requests. Negative tests generate
mutations that violate the OpenAPI schema (e.g., missing required fields,
wrong content types, invalid values) and verify the API returns a 4xx
status code.

=== When to Use

Use negative testing when you want to verify:

* Input validation is working correctly
* Required fields are enforced
* Content types are validated
* Invalid values are rejected

=== Example

@
import Haskemathesis.Check.Negative (negativeTestRejection)
import Haskemathesis.Gen.Negative (renderNegativeMutation)

-- In a property test
result = negativeTestRejection mutationDescription request response operation
@
-}
module Haskemathesis.Check.Negative (
    negativeTestRejection,
)
where

import Data.Text (Text)
import Haskemathesis.Check.Types (CheckResult (..), FailureDetail (..))
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

{- | Check that a negative test request was properly rejected.

This check verifies that the API returned a 4xx status code (client error)
for a request that was intentionally made invalid. If the API accepts
the invalid request (returns 2xx or 5xx), the check fails.

=== Parameters

* @mutation@ - Description of the mutation applied to make the request invalid
* @req@ - The mutated 'ApiRequest' that was sent
* @res@ - The 'ApiResponse' received from the API
* @op@ - The 'ResolvedOperation' being tested

=== Return Value

Returns 'CheckPassed' if the response status is 4xx, otherwise returns
'CheckFailed' with details about the failure.

=== Example

@
import Haskemathesis.Gen.Negative (genNegativeRequest, renderNegativeMutation)

-- In a property test
mReq <- forAll (genNegativeRequest operation)
case mReq of
    Nothing -> success
    Just (req, mutation) -> do
        res <- evalIO (execute req)
        case negativeTestRejection (renderNegativeMutation mutation) req res operation of
            CheckPassed -> success
            CheckFailed detail -> ...
@
-}
negativeTestRejection :: Text -> ApiRequest -> ApiResponse -> ResolvedOperation -> CheckResult
negativeTestRejection mutation req res op =
    if resStatusCode res >= 400 && resStatusCode res < 500
        then CheckPassed
        else
            CheckFailed
                FailureDetail
                    { fdCheck = "negative_test_rejection"
                    , fdMessage = "expected 4xx response for negative request"
                    , fdRequest = req
                    , fdResponse = res
                    , fdOperation = operationLabel op
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Nothing
                    , fdMutation = Just mutation
                    }

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
