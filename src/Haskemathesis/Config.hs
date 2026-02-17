{-# LANGUAGE StrictData #-}

module Haskemathesis.Config (
    TestConfig (..),
    defaultTestConfig,
    filterByOperationId,
    filterByPathPrefix,
    filterByTag,
) where

import Data.Text (Text)
import qualified Data.Text as T

import Haskemathesis.Auth.Config (AuthConfig)
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Check.Types (Check)
import Haskemathesis.Execute.Types (BaseUrl)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

-- | Configuration for test generation and execution.
data TestConfig = TestConfig
    { tcChecks :: ![Check]
    , tcAuthConfig :: !(Maybe AuthConfig)
    , tcBaseUrl :: !(Maybe BaseUrl)
    , tcPropertyCount :: !Int
    , tcOperationFilter :: !(ResolvedOperation -> Bool)
    }

-- | Sensible defaults.
defaultTestConfig :: TestConfig
defaultTestConfig =
    TestConfig
        { tcChecks = defaultChecks
        , tcAuthConfig = Nothing
        , tcBaseUrl = Nothing
        , tcPropertyCount = 100
        , tcOperationFilter = const True
        }

filterByOperationId :: Text -> ResolvedOperation -> Bool
filterByOperationId opId op = roOperationId op == Just opId

filterByPathPrefix :: Text -> ResolvedOperation -> Bool
filterByPathPrefix prefix op = T.isPrefixOf prefix (roPath op)

filterByTag :: Text -> ResolvedOperation -> Bool
filterByTag tag op = tag `elem` roTags op
