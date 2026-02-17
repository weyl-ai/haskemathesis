{-# LANGUAGE StrictData #-}

module Haskemathesis.Check.Types (
    Check (..),
    CheckResult (..),
    FailureDetail (..),
) where

import Data.Text (Text)

import Haskemathesis.Execute.Types (ApiRequest, ApiResponse)
import Haskemathesis.OpenApi.Types (ResolvedOperation)

data Check = Check
    { checkName :: Text
    , checkRun :: ApiRequest -> ApiResponse -> ResolvedOperation -> CheckResult
    }

data CheckResult
    = CheckPassed
    | CheckFailed FailureDetail
    deriving (Eq, Show)

data FailureDetail = FailureDetail
    { fdCheck :: Text
    , fdMessage :: Text
    , fdRequest :: ApiRequest
    , fdResponse :: ApiResponse
    , fdOperation :: Text
    , fdSchemaErrors :: [Text]
    , fdSchemaDiff :: Maybe Text
    }
    deriving (Eq, Show)
