{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Integration.Hspec
Description : Hspec test framework integration
Stability   : experimental

This module provides integration with the Hspec testing framework,
allowing you to run OpenAPI conformance tests as part of your Hspec
test suite. It supports both standard conformance testing and negative
testing modes.

==== Quick Start

@
import Test.Hspec
import Haskemathesis.Integration.Hspec
import Haskemathesis.Config

main :: IO ()
main = hspec $ specForAppWithConfig defaultConfig openApiSpec myApp
@

==== Streaming Endpoint Handling

WAI-based tests (in-memory, synchronous) cannot handle streaming endpoints
like Server-Sent Events (SSE) or NDJSON streams. These operations are
__automatically skipped__ when using 'specForApp' or 'specForAppWithConfig'.

To see which operations were skipped, use 'specForAppIO':

@
main :: IO ()
main = do
    (spec, skipped) <- specForAppIO defaultConfig openApiSpec myApp
    unless (null skipped) $
        hPutStrLn stderr $ "Skipped streaming operations: " ++ show skipped
    hspec spec
@

For streaming endpoints, use 'specForUrl' with a running HTTP server
which properly handles timeouts.
-}
module Haskemathesis.Integration.Hspec (
    -- * Standard testing (WAI Application)
    specForApp,
    specForAppWithConfig,

    -- * Standard testing (custom executor)
    specForExecutor,
    specForExecutorWithConfig,

    -- * Standard testing (HTTP client)
    specForUrl,

    -- * WAI with Streaming Info
    specForAppIO,
    specForAppNegativeIO,

    -- * Negative testing
    specForExecutorNegative,
    specForAppNegative,
    specForUrlNegative,
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
import Hedgehog (check)
import Network.HTTP.Client (Manager)
import Network.Wai (Application)
import System.IO (hPutStrLn, stderr)
import Test.Hspec (Spec, describe, it, shouldBe)

{- | Create an Hspec 'Spec' from pre-resolved operations with a custom executor.

This is the most flexible form, allowing you to provide your own request
executor function and pre-resolved operations. Use this when you need
fine-grained control over request execution.

==== Parameters

* @mBase@ - Optional base URL for error reporting
* @checks@ - List of conformance checks to run
* @execute@ - Function to execute API requests
* @ops@ - List of resolved operations to test

==== Example

@
spec :: Spec
spec = specForExecutor (Just "http://localhost:8080") checks myExecutor ops
@
-}
specForExecutor ::
    Maybe BaseUrl ->
    [Check] ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    Spec
specForExecutor mBase checks execute ops =
    describe "OpenAPI Conformance" $
        mapM_ (specForOperation mBase checks execute) ops

{- | Create an Hspec 'Spec' for a WAI 'Application'.

This is the simplest way to test a WAI application against an OpenAPI
specification. Operations are automatically resolved from the spec.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'specForAppIO' if you need to know which operations
were skipped.

==== Parameters

* @mBase@ - Optional base URL for error reporting
* @checks@ - List of conformance checks to run
* @openApi@ - The OpenAPI specification
* @app@ - The WAI application to test

==== Example

@
import Haskemathesis.Check.Standard (standardChecks)

spec :: Spec
spec = specForApp Nothing standardChecks openApiSpec myApp
@
-}
specForApp ::
    Maybe BaseUrl ->
    [Check] ->
    OpenApi ->
    Application ->
    Spec
specForApp mBase checks openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in specForExecutor mBase checks (executeWaiWithTimeout app) nonStreaming

{- | Create an Hspec 'Spec' with full configuration and custom executor.

This variant allows you to use a 'TestConfig' for fine-grained control
over test behavior including operation filtering, authentication, and
negative testing.

==== Parameters

* @openApi@ - The OpenAPI specification (used for request generation)
* @config@ - Test configuration
* @execute@ - Function to execute API requests
* @ops@ - List of resolved operations to test

==== Example

@
let config = defaultConfig { tcTestCount = 50 }
spec = specForExecutorWithConfig openApiSpec config myExecutor ops
@
-}
specForExecutorWithConfig ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    Spec
specForExecutorWithConfig openApi config execute ops =
    describe "OpenAPI Conformance" $
        mapM_ (specForOperationWithConfig openApi config execute) ops

{- | Create an Hspec 'Spec' for a WAI 'Application' with full configuration.

Combines the convenience of WAI application testing with the flexibility
of 'TestConfig'. This is the recommended approach for most use cases.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'specForAppIO' if you need to know which operations
were skipped.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @app@ - The WAI application to test

==== Example

@
import Haskemathesis.Config

spec :: Spec
spec = specForAppWithConfig defaultConfig openApiSpec myApp
@
-}
specForAppWithConfig ::
    TestConfig ->
    OpenApi ->
    Application ->
    Spec
specForAppWithConfig config openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in specForExecutorWithConfig openApi config (executeWaiWithTimeout app) nonStreaming

{- | Create an Hspec 'Spec' for testing a remote HTTP server.

This variant uses an HTTP client manager to make real network requests
to a running server. Useful for integration testing against deployed
services or external APIs.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @manager@ - HTTP client manager for making requests
* @baseUrl@ - Base URL of the server to test

==== Example

@
import Network.HTTP.Client (newManager, defaultManagerSettings)

spec :: Spec
spec = do
   manager <- runIO $ newManager defaultManagerSettings
   specForUrl defaultConfig openApiSpec manager "http://localhost:8080"
@
-}
specForUrl ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    Spec
specForUrl config openApi manager baseUrl =
    let config' = config{tcBaseUrl = Just baseUrl}
     in specForExecutorWithConfig openApi config' (executeHttpWithTimeout manager baseUrl) (resolveOperations openApi)

{- | Create an Hspec 'Spec' for negative testing with a custom executor.

Negative testing sends intentionally malformed requests to verify that
the API correctly rejects invalid input. This helps ensure proper input
validation and error handling.

==== Parameters

* @openApi@ - The OpenAPI specification
* @config@ - Test configuration (negative testing is enabled automatically)
* @execute@ - Function to execute API requests
* @ops@ - List of resolved operations to test

==== What is tested

* Missing required parameters (path, query, header)
* Invalid parameter values (wrong types)
* Invalid request bodies
* Wrong content types
-}
specForExecutorNegative ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    [ResolvedOperation] ->
    Spec
specForExecutorNegative openApi config execute ops =
    let config' = config{tcNegativeTesting = True}
     in specForExecutorWithConfig openApi config' execute ops

{- | Create an Hspec 'Spec' for negative testing of a WAI 'Application'.

Convenience wrapper around 'specForExecutorNegative' for WAI applications.

__Note:__ Streaming operations (SSE, NDJSON) are automatically filtered
out because WAI testing is synchronous and cannot handle streaming
responses. Use 'specForAppNegativeIO' if you need to know which operations
were skipped.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @app@ - The WAI application to test

==== Example

@
-- Test that invalid requests are properly rejected
negativeSpec :: Spec
negativeSpec = specForAppNegative defaultConfig openApiSpec myApp
@
-}
specForAppNegative ::
    TestConfig ->
    OpenApi ->
    Application ->
    Spec
specForAppNegative config openApi app =
    let allOps = resolveOperations openApi
        (nonStreaming, _skipped) = partitionStreamingOps allOps
     in specForExecutorNegative openApi config (executeWaiWithTimeout app) nonStreaming

{- | Create an Hspec 'Spec' for negative testing of a remote HTTP server.

Convenience wrapper around 'specForExecutorNegative' for HTTP testing.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @manager@ - HTTP client manager for making requests
* @baseUrl@ - Base URL of the server to test

==== Example

@
-- Test that the remote server rejects invalid requests
negativeSpec :: Spec
negativeSpec = do
   manager <- runIO $ newManager defaultManagerSettings
   specForUrlNegative defaultConfig openApiSpec manager "http://localhost:8080"
@
-}
specForUrlNegative ::
    TestConfig ->
    OpenApi ->
    Manager ->
    BaseUrl ->
    Spec
specForUrlNegative config openApi manager baseUrl =
    specForExecutorNegative openApi (config{tcBaseUrl = Just baseUrl}) (executeHttpWithTimeout manager baseUrl) (resolveOperations openApi)

specForOperation ::
    Maybe BaseUrl ->
    [Check] ->
    ExecutorWithTimeout ->
    ResolvedOperation ->
    Spec
specForOperation mBase checks execute op =
    it (T.unpack (operationLabel op)) $
        check (propertyForOperation mBase checks execute op) >>= (`shouldBe` True)

specForOperationWithConfig ::
    OpenApi ->
    TestConfig ->
    ExecutorWithTimeout ->
    ResolvedOperation ->
    Spec
specForOperationWithConfig openApi config execute op
    | not (tcOperationFilter config op) = pure ()
    | otherwise =
        it (T.unpack (operationLabel op)) $
            check (propertyForOperationWithConfig openApi config execute op) >>= (`shouldBe` True)

-- -----------------------------------------------------------------------------
-- WAI IO variants (with streaming operation info)
-- -----------------------------------------------------------------------------

{- | Create an Hspec 'Spec' from a WAI 'Application', returning skipped operations.

Like 'specForAppWithConfig', but returns the list of operations that were
skipped due to being streaming endpoints. Also prints a warning to stderr
if any operations are skipped.

Streaming operations (SSE, NDJSON) cannot be tested with WAI because WAI
testing is synchronous and in-memory. For streaming endpoints, use
'specForUrl' with a running HTTP server.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @app@ - The WAI application to test

==== Return Value

Returns a tuple of:

* The Hspec 'Spec' for non-streaming operations
* List of operation labels that were skipped (streaming operations)

==== Example

@
main :: IO ()
main = do
    (spec, skipped) <- specForAppIO defaultConfig openApiSpec myApp
    unless (null skipped) $
        putStrLn $ "Note: " ++ show (length skipped) ++ " streaming ops skipped"
    hspec spec
@
-}
specForAppIO ::
    TestConfig ->
    OpenApi ->
    Application ->
    IO (Spec, [Text])
specForAppIO config openApi app = do
    let allOps = resolveOperations openApi
        (nonStreaming, streaming) = partitionStreamingOps allOps
        skippedLabels = map operationLabel streaming
        spec = specForExecutorWithConfig openApi config (executeWaiWithTimeout app) nonStreaming
    warnSkippedStreaming skippedLabels
    pure (spec, skippedLabels)

{- | Create a negative testing 'Spec' from a WAI 'Application', returning skipped operations.

Like 'specForAppNegative', but returns the list of operations that were
skipped due to being streaming endpoints. Also prints a warning to stderr
if any operations are skipped.

==== Parameters

* @config@ - Test configuration
* @openApi@ - The OpenAPI specification
* @app@ - The WAI application to test

==== Return Value

Returns a tuple of:

* The Hspec 'Spec' for non-streaming operations (negative tests)
* List of operation labels that were skipped (streaming operations)
-}
specForAppNegativeIO ::
    TestConfig ->
    OpenApi ->
    Application ->
    IO (Spec, [Text])
specForAppNegativeIO config openApi app = do
    let allOps = resolveOperations openApi
        (nonStreaming, streaming) = partitionStreamingOps allOps
        skippedLabels = map operationLabel streaming
        spec = specForExecutorNegative openApi config (executeWaiWithTimeout app) nonStreaming
    warnSkippedStreaming skippedLabels
    pure (spec, skippedLabels)

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
    hPutStrLn stderr "  Use specForUrl with a live HTTP server to test streaming endpoints."
