{-# LANGUAGE OverloadedStrings #-}

{- | Tasty integration helpers for generated properties.

This module provides convenient functions for integrating Haskemathesis
properties with the Tasty test framework. It handles the conversion
of generated properties into Tasty 'TestTree' values.

=== Basic Usage

Create a Tasty test suite from an OpenAPI spec:

@
import Test.Tasty (defaultMain)
import Haskemathesis.Integration.Tasty (testTreeForApp)
import Haskemathesis.Check.Standard (defaultChecks)
import Data.OpenApi (OpenApi)

main :: IO ()
main = do
    spec <- loadOpenApiFile "api.yaml"
    let app = myWaiApplication
        tests = testTreeForApp Nothing defaultChecks spec app
    defaultMain tests
@

=== With Custom Configuration

For more control, use the configuration-based functions:

@
import Haskemathesis.Config

myConfig :: TestConfig
myConfig = defaultTestConfig
    { tcPropertyCount = 200
    , tcNegativeTesting = True
    }

tests = testTreeForAppWithConfig myConfig spec app
@

=== Testing Remote APIs

Test a running HTTP server:

@
import Network.HTTP.Client (newManager, defaultManagerSettings)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    spec <- loadOpenApiFile "api.yaml"
    let tests = testTreeForUrl defaultTestConfig spec manager "http://localhost:8080"
    defaultMain tests
@

=== Streaming Endpoint Handling

WAI-based tests (in-memory, synchronous) cannot handle streaming endpoints
like Server-Sent Events (SSE) or NDJSON streams. These operations are
__automatically skipped__ when using 'testTreeForApp' or 'testTreeForAppWithConfig'.

To see which operations were skipped, use 'testTreeForAppIO':

@
main :: IO ()
main = do
    spec <- loadOpenApiFile "api.yaml"
    (tests, skipped) <- testTreeForAppIO defaultTestConfig spec myApp
    unless (null skipped) $
        hPutStrLn stderr $ "Skipped streaming operations: " ++ show skipped
    defaultMain tests
@

For streaming endpoints, use 'testTreeForUrl' with a running HTTP server
which properly handles timeouts.
-}
module Haskemathesis.Integration.Tasty (
    -- * Basic Test Trees
    testTreeForExecutor,
    testTreeForApp,

    -- * Configuration-Based Test Trees
    testTreeForExecutorWithConfig,
    testTreeForAppWithConfig,
    testTreeForUrl,

    -- * WAI with Streaming Info
    testTreeForAppIO,
    testTreeForAppNegativeIO,

    -- * Negative Testing
    testTreeForExecutorNegative,
    testTreeForAppNegative,
    testTreeForUrlNegative,
)
where

import Data.List (partition)
import Data.OpenApi (OpenApi)
import Data.Text (Text)
import qualified Data.Text as T
import Haskemathesis.Check.Standard.Helpers (operationLabel)
import Haskemathesis.Check.Types (Check)
import Haskemathesis.Config (TestConfig (..))
import Haskemathesis.Execute.Http (executeHttpWithTimeout)
import Haskemathesis.Execute.Types (BaseUrl, ExecutorWithTimeout)
import Haskemathesis.Execute.Wai (executeWaiWithTimeout)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertyForOperation, propertyForOperationWithConfig)
import Hedgehog.Internal.Property (PropertyName (..))
import Network.HTTP.Client (Manager)
import Network.Wai (Application)
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

{- | Create a Tasty 'TestTree' from a custom executor.

This is the most flexible way to create tests, allowing you to
provide any function that can execute 'ApiRequest' values.

=== Parameters

* @mBase@ - Optional base URL for failure reports
* @checks@ - List of 'Check' functions to validate responses
* @execute@ - Your custom executor function
* @ops@ - List of operations to test (from 'resolveOperations')

=== Example

@
myExecutor :: ApiRequest -> IO ApiResponse
myExecutor req = ... -- your custom logic

tests = testTreeForExecutor Nothing defaultChecks myExecutor ops
@
-}
testTreeForExecutor ::
    Maybe BaseUrl ->
    [Check] ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutor mBase checks execute ops =
    testGroup
        "OpenAPI Conformance"
        [ testPropertyNamed name (PropertyName name) (propertyForOperation mBase checks execute op)
        | op <- ops
        , let name = T.unpack (operationLabel op)
        ]

{- | Create a Tasty 'TestTree' from a WAI 'Application'.

This is a convenient way to test a WAI application (e.g., Servant,
Yesod, or any other WAI-based framework) without starting a
real HTTP server.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'testTreeForAppIO' if you need to know which operations
were skipped.

=== Parameters

* @mBase@ - Optional base URL for failure reports
* @checks@ - List of 'Check' functions to validate responses
* @openApi@ - The OpenAPI specification
* @app@ - Your WAI 'Application'

=== Example

@
import Network.Wai (Application)

myApp :: Application
myApp = ... -- your WAI application

main :: IO ()
main = do
    spec <- loadOpenApiFile "api.yaml"
    let tests = testTreeForApp Nothing defaultChecks spec myApp
    defaultMain tests
@
-}
testTreeForApp ::
    Maybe BaseUrl ->
    [Check] ->
    OpenApi ->
    Application ->
    TestTree
testTreeForApp mBase checks openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in testTreeForExecutor mBase checks (executeWaiWithTimeout app) nonStreaming

{- | Create a Tasty 'TestTree' with full configuration support.

This function provides the most flexibility for creating test trees.
It accepts a 'TestConfig' for authentication, filtering, and
other advanced options.

=== Parameters

* @openApi@ - The OpenAPI specification
* @config@ - The 'TestConfig' controlling test generation
* @execute@ - Function to execute requests
* @ops@ - List of operations to test

=== Example

@
config = defaultTestConfig
    { tcPropertyCount = 200
    , tcAuthConfig = Just myAuthConfig
    }

tests = testTreeForExecutorWithConfig spec config executor ops
@
-}
testTreeForExecutorWithConfig ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutorWithConfig openApi config execute ops =
    testGroup
        "OpenAPI Conformance"
        [ testPropertyNamed name (PropertyName name) (propertyForOperationWithConfig openApi config execute op)
        | op <- ops
        , tcOperationFilter config op
        , let name = T.unpack (operationLabel op)
        ]

{- | Create a Tasty 'TestTree' from a WAI 'Application' with configuration.

Combines 'testTreeForExecutorWithConfig' with 'executeWai' for
convenient testing of WAI applications.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'testTreeForAppIO' if you need to know which operations
were skipped.

=== Parameters

* @config@ - The 'TestConfig' controlling test generation
* @openApi@ - The OpenAPI specification
* @app@ - Your WAI 'Application'

=== Example

@
config = defaultTestConfig { tcPropertyCount = 200 }
tests = testTreeForAppWithConfig config spec myApp
@
-}
testTreeForAppWithConfig ::
    TestConfig ->
    OpenApi ->
    Application ->
    TestTree
testTreeForAppWithConfig config openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in testTreeForExecutorWithConfig openApi config (executeWaiWithTimeout app) nonStreaming

{- | Create a Tasty 'TestTree' for testing a running HTTP server.

This function creates tests that execute against a live HTTP server
at the specified base URL.

=== Parameters

* @config@ - The 'TestConfig' controlling test generation
* @openApi@ - The OpenAPI specification
* @manager@ - HTTP connection manager
* @baseUrl@ - Base URL of the running server (e.g., "http://localhost:8080")

=== Example

@
import Network.HTTP.Client (newManager, defaultManagerSettings)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    spec <- loadOpenApiFile "api.yaml"
    let tests = testTreeForUrl defaultTestConfig spec manager "http://localhost:8080"
    defaultMain tests
@
-}
testTreeForUrl ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    TestTree
testTreeForUrl config openApi manager baseUrl =
    let config' = config{tcBaseUrl = Just baseUrl}
     in testTreeForExecutorWithConfig openApi config' (executeHttpWithTimeout manager baseUrl) (resolveOperations openApi)

{- | Create a Tasty 'TestTree' for negative testing with a custom executor.

Negative testing generates invalid requests to verify that the API
properly rejects them with appropriate error responses.

=== Parameters

* @openApi@ - The OpenAPI specification
* @config@ - The 'TestConfig' (negative testing flag is ignored, always enabled)
* @execute@ - Function to execute requests
* @ops@ - List of operations to test

=== Example

@
-- Test that invalid requests are properly rejected
negTests = testTreeForExecutorNegative spec config executor ops
@
-}
testTreeForExecutorNegative ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    TestTree
testTreeForExecutorNegative openApi config execute ops =
    let config' = config{tcNegativeTesting = True}
     in testTreeForExecutorWithConfig openApi config' execute ops

{- | Create a Tasty 'TestTree' for negative testing with a WAI application.

Convenience function that combines 'testTreeForExecutorNegative' with
'executeWai' for testing WAI applications.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'testTreeForAppNegativeIO' if you need to know which
operations were skipped.

=== Parameters

* @config@ - The 'TestConfig'
* @openApi@ - The OpenAPI specification
* @app@ - Your WAI 'Application'

=== Example

@
negTests = testTreeForAppNegative defaultTestConfig spec myApp
@
-}
testTreeForAppNegative ::
    TestConfig ->
    OpenApi ->
    Application ->
    TestTree
testTreeForAppNegative config openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in testTreeForExecutorNegative openApi config (executeWaiWithTimeout app) nonStreaming

{- | Create a Tasty 'TestTree' for negative testing against a live server.

Convenience function for running negative tests against a running
HTTP server at the specified base URL.

=== Parameters

* @config@ - The 'TestConfig'
* @openApi@ - The OpenAPI specification
* @manager@ - HTTP connection manager
* @baseUrl@ - Base URL of the running server

=== Example

@
import Network.HTTP.Client (newManager, defaultManagerSettings)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    spec <- loadOpenApiFile "api.yaml"
    let negTests = testTreeForUrlNegative defaultTestConfig spec manager "http://localhost:8080"
    defaultMain negTests
@
-}
testTreeForUrlNegative ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    TestTree
testTreeForUrlNegative config openApi manager baseUrl =
    testTreeForExecutorNegative openApi (config{tcBaseUrl = Just baseUrl}) (executeHttpWithTimeout manager baseUrl) (resolveOperations openApi)

-- -----------------------------------------------------------------------------
-- WAI IO variants (with streaming operation info)
-- -----------------------------------------------------------------------------

{- | Create a Tasty 'TestTree' from a WAI 'Application', returning skipped operations.

Like 'testTreeForAppWithConfig', but returns the list of operations that were
skipped due to being streaming endpoints. Also prints a warning to stderr
if any operations are skipped.

Streaming operations (SSE, NDJSON) cannot be tested with WAI because WAI
testing is synchronous and in-memory. For streaming endpoints, use
'testTreeForUrl' with a running HTTP server.

=== Parameters

* @config@ - The 'TestConfig' controlling test generation
* @openApi@ - The OpenAPI specification
* @app@ - Your WAI 'Application'

=== Return Value

Returns a tuple of:

* The 'TestTree' for non-streaming operations
* List of operation labels that were skipped (streaming operations)

=== Example

@
main :: IO ()
main = do
    spec <- loadOpenApiFile "api.yaml"
    (tests, skipped) <- testTreeForAppIO defaultTestConfig spec myApp
    unless (null skipped) $
        putStrLn $ "Note: " ++ show (length skipped) ++ " streaming ops skipped"
    defaultMain tests
@
-}
testTreeForAppIO ::
    TestConfig ->
    OpenApi ->
    Application ->
    IO (TestTree, [Text])
testTreeForAppIO config openApi app = do
    let allOps = resolveOperations openApi
        (nonStreaming, streaming) = partitionStreamingOps allOps
        skippedLabels = map operationLabel streaming
        tree = testTreeForExecutorWithConfig openApi config (executeWaiWithTimeout app) nonStreaming
    warnSkippedStreaming skippedLabels
    pure (tree, skippedLabels)

{- | Create a negative testing 'TestTree' from a WAI 'Application', returning skipped operations.

Like 'testTreeForAppNegative', but returns the list of operations that were
skipped due to being streaming endpoints. Also prints a warning to stderr
if any operations are skipped.

=== Parameters

* @config@ - The 'TestConfig'
* @openApi@ - The OpenAPI specification
* @app@ - Your WAI 'Application'

=== Return Value

Returns a tuple of:

* The 'TestTree' for non-streaming operations (negative tests)
* List of operation labels that were skipped (streaming operations)
-}
testTreeForAppNegativeIO ::
    TestConfig ->
    OpenApi ->
    Application ->
    IO (TestTree, [Text])
testTreeForAppNegativeIO config openApi app = do
    let allOps = resolveOperations openApi
        (nonStreaming, streaming) = partitionStreamingOps allOps
        skippedLabels = map operationLabel streaming
        tree = testTreeForExecutorNegative openApi config (executeWaiWithTimeout app) nonStreaming
    warnSkippedStreaming skippedLabels
    pure (tree, skippedLabels)

-- -----------------------------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------------------------

-- | Partition operations into (non-streaming, streaming).
partitionStreamingOps :: [ResolvedOperation] -> ([ResolvedOperation], [ResolvedOperation])
partitionStreamingOps = partition (not . roIsStreaming)

-- | Print a warning to stderr if any streaming operations were skipped.
warnSkippedStreaming :: [Text] -> IO ()
warnSkippedStreaming [] = pure ()
warnSkippedStreaming skipped = do
    let count = foldl' (\n _ -> n + 1) (0 :: Int) skipped
    hPutStrLn stderr $
        "[haskemathesis] Skipping "
            ++ show count
            ++ " streaming operation(s) (WAI executor cannot handle streaming):"
    mapM_ (hPutStrLn stderr . ("  - " ++) . T.unpack) skipped
    hPutStrLn stderr "  Use testTreeForUrl with a live HTTP server to test streaming endpoints."
