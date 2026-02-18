{-# LANGUAGE StrictData #-}

{- | Top-level configuration for property generation and execution.

This module provides 'TestConfig' for customizing test generation
and execution behavior, along with helper functions for filtering
operations.

=== Basic Usage

Use 'defaultTestConfig' as a starting point and override specific fields:

@
myConfig :: TestConfig
myConfig = defaultTestConfig
    { tcPropertyCount = 50
    , tcNegativeTesting = True
    }
@

=== Filtering Operations

You can filter which OpenAPI operations to test using 'tcOperationFilter':

@
filterConfig :: TestConfig
filterConfig = defaultTestConfig
    { tcOperationFilter = filterByTag "public"
    }
@
-}
module Haskemathesis.Config (
    TestConfig (..),
    defaultTestConfig,
    filterByOperationId,
    filterByPathPrefix,
    filterByTag,
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Haskemathesis.Auth.Config (AuthConfig)
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Check.Types (Check)
import Haskemathesis.Execute.Types (BaseUrl)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

{- | Configuration for test generation and execution.

This record controls how properties are generated, how many tests to run,
authentication settings, and which operations to include.

=== Fields

* 'tcChecks' - List of response checks to apply to each test
* 'tcAuthConfig' - Optional authentication configuration
* 'tcBaseUrl' - Optional base URL for rendering failure reports
* 'tcPropertyCount' - Number of test cases to generate per property
* 'tcNegativeTesting' - Whether to generate negative test cases
* 'tcOperationFilter' - Predicate to filter which operations to test
-}
data TestConfig = TestConfig
    { tcChecks :: ![Check]
    , tcAuthConfig :: !(Maybe AuthConfig)
    , tcBaseUrl :: !(Maybe BaseUrl)
    , tcPropertyCount :: !Int
    , tcNegativeTesting :: !Bool
    , tcOperationFilter :: !(ResolvedOperation -> Bool)
    }

{- | Sensible defaults for 'TestConfig'.

The default configuration:

* Uses 'defaultChecks' (server errors, schema conformance, status codes)
* No authentication
* No base URL for reports
* 100 properties per operation
* Negative testing disabled
* All operations included (filter always returns 'True')
-}
defaultTestConfig :: TestConfig
defaultTestConfig =
    TestConfig
        { tcChecks = defaultChecks
        , tcAuthConfig = Nothing
        , tcBaseUrl = Nothing
        , tcPropertyCount = 100
        , tcNegativeTesting = False
        , tcOperationFilter = const True
        }

{- | Create a filter that matches a specific operation ID.

Useful when you want to test a single endpoint:

@
config = defaultTestConfig
    { tcOperationFilter = filterByOperationId "getUserById"
    }
@
-}
filterByOperationId :: Text -> ResolvedOperation -> Bool
filterByOperationId opId op = roOperationId op == Just opId

{- | Create a filter that matches operations by path prefix.

Useful for testing a subset of your API:

@
config = defaultTestConfig
    { tcOperationFilter = filterByPathPrefix "/api/v2/"
    }
@
-}
filterByPathPrefix :: Text -> ResolvedOperation -> Bool
filterByPathPrefix prefix op = T.isPrefixOf prefix (roPath op)

{- | Create a filter that matches operations by tag.

Useful for testing operations by functional area:

@
config = defaultTestConfig
    { tcOperationFilter = filterByTag "public"
    }
@
-}
filterByTag :: Text -> ResolvedOperation -> Bool
filterByTag tag op = tag `elem` roTags op
