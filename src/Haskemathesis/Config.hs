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

=== Stateful Testing

Enable stateful testing to chain operations together:

@
statefulConfig :: TestConfig
statefulConfig = defaultTestConfig
    { tcStatefulTesting = True
    , tcMaxSequenceLength = 5
    , tcStatefulChecks = defaultStatefulChecks
    }
@
-}
module Haskemathesis.Config (
    TestConfig (..),
    defaultTestConfig,
    filterByOperationId,
    filterByPathPrefix,
    filterByTag,
    defaultStatefulChecks,
)
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Haskemathesis.Auth.Config (AuthConfig)
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Check.Types (Check)
import Haskemathesis.Execute.Types (BaseUrl)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Stateful.Checks (
    StatefulCheck,
    ensureResourceAvailability,
    useAfterFree,
 )
import Network.HTTP.Types (HeaderName)

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
* 'tcHeaders' - Global headers to include in every request
* 'tcStreamingTimeout' - Default timeout for streaming endpoints (milliseconds)
* 'tcStatefulTesting' - Whether to enable stateful testing
* 'tcStatefulChecks' - List of stateful checks to run after sequences
* 'tcMaxSequenceLength' - Maximum number of operations per sequence
* 'tcCleanupOnFailure' - Whether to run cleanup steps even on failure
-}
data TestConfig = TestConfig
    { tcChecks :: ![Check]
    , tcAuthConfig :: !(Maybe AuthConfig)
    , tcBaseUrl :: !(Maybe BaseUrl)
    , tcPropertyCount :: !Int
    , tcNegativeTesting :: !Bool
    , tcOperationFilter :: !(ResolvedOperation -> Bool)
    , tcHeaders :: ![(HeaderName, ByteString)]
    , tcStreamingTimeout :: !(Maybe Int)
    {- ^ Default timeout in milliseconds for streaming endpoints.

    This timeout applies to operations that are detected as streaming
    (have @text/event-stream@ or @application/x-ndjson@ content types)
    and don't have an explicit @x-timeout@ set in the OpenAPI spec.

    __Timeout precedence:__

    1. Operation's @x-timeout@ extension (if set)
    2. 'tcStreamingTimeout' (if operation is streaming)
    3. No timeout (use HTTP client default)

    __Recommended values:__

    * 500-2000ms for quick test suites
    * 5000ms or more if you need to capture meaningful streaming data

    Set to 'Nothing' to use the HTTP client's default timeout for
    streaming endpoints (which may cause tests to hang).
    -}
    , tcStatefulTesting :: !Bool
    {- ^ Enable stateful testing.

    When enabled, 'propertiesForSpecWithConfig' will also generate
    stateful properties that test operation sequences. Use
    'propertyStateful' for direct access to stateful testing.

    Default: 'False'
    -}
    , tcStatefulChecks :: ![StatefulCheck]
    {- ^ Stateful checks to run after each sequence.

    These checks verify stateful properties like:

    * 'useAfterFree' - Deleted resources return 404
    * 'ensureResourceAvailability' - Created resources are accessible

    Default: 'defaultStatefulChecks' (both of the above)
    -}
    , tcMaxSequenceLength :: !Int
    {- ^ Maximum number of operations per stateful sequence.

    Controls how long generated sequences can be. Longer sequences
    test more complex scenarios but take longer to run and may be
    harder to debug when they fail.

    Default: 5
    -}
    , tcCleanupOnFailure :: !Bool
    {- ^ Whether to run cleanup steps even when the main sequence fails.

    When 'True', cleanup operations (typically DELETEs) are executed
    even if an earlier step in the sequence fails. This prevents
    resource leaks but may make debugging harder.

    Default: 'True'
    -}
    }

{- | Sensible defaults for 'TestConfig'.

The default configuration:

* Uses 'defaultChecks' (server errors, schema conformance, status codes)
* No authentication
* No base URL for reports
* 100 properties per operation
* Negative testing disabled
* All operations included (filter always returns 'True')
* No global headers
* 1 second (1000ms) streaming timeout
* Stateful testing disabled
* Default stateful checks (use-after-free, resource availability)
* Max sequence length of 5
* Cleanup on failure enabled
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
        , tcHeaders = []
        , tcStreamingTimeout = Just 1000
        , tcStatefulTesting = False
        , tcStatefulChecks = defaultStatefulChecks
        , tcMaxSequenceLength = 5
        , tcCleanupOnFailure = True
        }

{- | Default stateful checks.

Includes:

* 'useAfterFree' - Verifies deleted resources return 404
* 'ensureResourceAvailability' - Verifies created resources are accessible

Use these as a starting point and add or remove checks as needed:

@
myChecks = defaultStatefulChecks ++ [ensureModificationPersisted]
@
-}
defaultStatefulChecks :: [StatefulCheck]
defaultStatefulChecks =
    [ useAfterFree
    , ensureResourceAvailability
    ]

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
