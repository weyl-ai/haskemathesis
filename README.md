# `haskemathesis`

Property-based API testing driven by OpenAPI 3.x schemas, powered by Hedgehog, inspired by python's [`schemathesis`](https://github.com/schemathesis/schemathesis).

This library loads an OpenAPI spec, generates valid requests from the schema,
executes them against either a WAI `Application` or a live HTTP endpoint, and
applies a configurable set of checks to the responses.

## Features

- OpenAPI 3.0 schema parsing and operation resolution
- Schema-driven generators for JSON requests
- WAI executor (no network) and HTTP executor
- Built-in checks (status codes, content type, response schema, response headers)
- Curl rendering for failure reproduction
- Hspec and Tasty integration helpers

## Quick Start (WAI)

1. Create an OpenAPI spec file (YAML or JSON).
1. Use the integration helpers to generate tests for your application.

```haskell
import Test.Hspec (hspec)

import Haskemathesis.Check.Standard (allChecks)
import Haskemathesis.Execute.Wai (executeWai)
import Haskemathesis.Integration.Hspec (specForExecutor)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)

main :: IO ()
main = do
  specResult <- loadOpenApiFile "openapi.yaml"
  case specResult of
    Left err -> error (show err)
    Right spec ->
      hspec (specForExecutor Nothing allChecks (executeWai app) (resolveOperations spec))
```

## Quick Start (HTTP)

```haskell
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Tasty (defaultMain)

import Haskemathesis.Config (defaultTestConfig)
import Haskemathesis.Integration.Tasty (testTreeForUrl)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  specResult <- loadOpenApiFile "openapi.yaml"
  case specResult of
    Left err -> error (show err)
    Right spec ->
      defaultMain (testTreeForUrl defaultTestConfig spec manager "https://api.example.com")
```

## Auth & Operation Filters

```haskell
import qualified Data.Map.Strict as Map

import Haskemathesis.Auth.Config (AuthConfig(..), AuthValue(..))
import Haskemathesis.Config (defaultTestConfig, filterByTag)
import Haskemathesis.Integration.Hspec (specForUrl)

let authConfig =
      AuthConfig
        (Map.fromList [("api_key", AuthApiKey "secret-token")])
let config =
      defaultTestConfig
        { tcAuthConfig = Just authConfig
        , tcOperationFilter = filterByTag "public"
        }
```

## Negative Testing

Enable negative testing (mutations that should result in a 4xx response) by
turning on `tcNegativeTesting` or using the `*Negative` helpers.

```haskell
import Haskemathesis.Config (defaultTestConfig)
import Haskemathesis.Integration.Hspec (specForAppNegative)

let config = defaultTestConfig { tcNegativeTesting = True }
hspec (specForAppNegative config spec app)
```

## Examples

The repo includes runnable examples:

- `examples/HspecExample.hs`
- `examples/TastyExample.hs`
- `examples/HspecNegativeExample.hs`
- `examples/openapi.yaml`

Run them with cabal:

```bash
cabal run haskemathesis-hspec-example
cabal run haskemathesis-hspec-negative-example
cabal run haskemathesis-tasty-example
```

## Checks

The default check set includes:

- status code conformance
- content type conformance (including +json media types)
- response schema conformance
- required response headers
- response header schema validation
- not-a-server-error (5xx guard)

Use `Haskemathesis.Check.Standard.allChecks` or `defaultChecks`.

## Development

Run tests:

```bash
cabal test
```

Nix users can use the devshell or `nix build` / `nix flake check`.

## OpenAPI Coverage

- OpenAPI 3.0.x is supported via the `openapi3` library.
- OpenAPI 3.1 is not parsed natively; use a 3.1 → 3.0 preprocessing step if
  you need 3.1 features (e.g., `null` in type arrays or full JSON Schema 2020-12).
