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

    -- * Negative testing
    specForExecutorNegative,
    specForAppNegative,
    specForUrlNegative,
)
where

import Data.OpenApi (OpenApi)
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
    specForExecutor mBase checks (executeWaiWithTimeout app) (resolveOperations openApi)

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
    specForExecutorWithConfig openApi config (executeWaiWithTimeout app) (resolveOperations openApi)

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
    specForExecutorNegative openApi config (executeWaiWithTimeout app) (resolveOperations openApi)

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
