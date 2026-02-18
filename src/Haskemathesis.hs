{- |
Module      : Haskemathesis
Description : OpenAPI 3.0 Property-Based Testing
Copyright   : (c) 2026 Weyl AI
License     : MIT
Maintainer  : luke@weyl.ai
Stability   : experimental

= Haskemathesis: OpenAPI Property-Based Testing

Haskemathesis is a property-based testing library that automatically generates test cases
from your OpenAPI 3.0 specification. It validates that your API implementation conforms
to its specification by generating random requests and verifying the responses.

== Key Features

* __Automatic Request Generation__: Generates valid random requests based on your OpenAPI schema
* __Comprehensive Validation__: Validates status codes, response bodies, headers, and content types
* __Negative Testing__: Automatically generates invalid requests to test error handling
* __Framework Integration__: Seamlessly integrates with Hspec and Tasty
* __Flexible Execution__: Test WAI applications directly or remote HTTP servers

== Quick Start

=== 1. Define your API

Ensure you have an OpenAPI 3.0 specification for your API. This can be a YAML or JSON file,
or generated from code.

=== 2. Create a Test Suite

Use the integration modules to create a test suite. Here's an example using Hspec and a WAI application:

@
import Test.Hspec
import Haskemathesis.Integration.Hspec
import Haskemathesis.Config (defaultConfig)
import Haskemathesis.OpenApi.Loader (loadOpenApi)
import MyApp (myApp) -- Your WAI application

main :: IO ()
main = do
    -- Load the OpenAPI specification
    openApi <- loadOpenApi "openapi.yaml"

    hspec $ do
        describe "API Conformance" $ do
            -- Test all operations defined in the spec
            specForApp defaultConfig openApi myApp

            -- Run negative tests (send invalid data)
            specForAppNegative defaultConfig openApi myApp
@

== Configuration

You can customize the testing behavior using 'TestConfig':

@
import Haskemathesis.Config

myConfig :: TestConfig
myConfig = defaultConfig
    { tcTestCount = 50           -- Number of tests per operation
    , tcExcludeOperations =      -- Skip specific operations
        [ "deleteUser"           -- By operationId
        , "POST /admin/reset"    -- By method and path
        ]
    , tcHeaders =                -- Add global headers (e.g., auth)
        [ ("Authorization", "Bearer token123") ]
    }
@

== Core Concepts

=== Generators

Haskemathesis uses 'Hedgehog' to generate random data. The generation logic is handled by:

* "Haskemathesis.Gen.Request" - Generates full API requests
* "Haskemathesis.Gen.Core" - Generates JSON values from schemas
* "Haskemathesis.Gen.Negative" - Generates invalid requests for negative testing

=== Checks

Validation is performed by 'Check' functions. The standard checks are:

* "Haskemathesis.Check.Standard.Status" - Validates status codes
* "Haskemathesis.Check.Standard.ResponseSchema" - Validates response bodies against schemas
* "Haskemathesis.Check.Standard.Headers" - Validates response headers
* "Haskemathesis.Check.Standard.ContentType" - Validates Content-Type headers

=== Integrations

* "Haskemathesis.Integration.Hspec" - Integration with Hspec
* "Haskemathesis.Integration.Tasty" - Integration with Tasty
-}
module Haskemathesis (
    -- * Configuration
    module Haskemathesis.Config,

    -- * Integration
    module Haskemathesis.Integration.Hspec,
    module Haskemathesis.Integration.Tasty,

    -- * Loading Specifications
    module Haskemathesis.OpenApi.Loader,
)
where

import Haskemathesis.Config
import Haskemathesis.Integration.Hspec
import Haskemathesis.Integration.Tasty
import Haskemathesis.OpenApi.Loader
