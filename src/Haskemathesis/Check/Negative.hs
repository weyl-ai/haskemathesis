{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Negative (
    negativeTestRejection,
) where

import Data.Text (Text)

import Haskemathesis.Check.Types (CheckResult (..), FailureDetail (..))
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

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
