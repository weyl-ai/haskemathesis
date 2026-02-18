# `haskemathesis`

Property-based API testing driven by OpenAPI 3.x schemas, powered by Hedgehog, inspired by python's [`schemathesis`](https://github.com/schemathesis/schemathesis).

This library loads an OpenAPI spec, generates valid requests from the schema,
executes them against either a WAI `Application` or a live HTTP endpoint, and
applies a configurable set of checks to the responses.

## Features

- OpenAPI 3.0 and 3.1 schema parsing and operation resolution
- Schema-driven generators for JSON requests
- WAI executor (no network) and HTTP executor
- Built-in checks (status codes, content type, response schema, response headers)
- Curl rendering for failure reproduction
- Hspec and Tasty integration helpers
- Standalone CLI for testing any HTTP API without writing Haskell

## CLI

The CLI allows you to test any HTTP API without writing Haskell code.

### Installation

```bash
cabal install haskemathesis
```

Or run directly with cabal:

```bash
cabal run haskemathesis-cli -- <command> [options]
```

### Commands

#### `test` - Run property-based tests against an API

```bash
# Basic usage
haskemathesis-cli test --spec openapi.yaml --url http://localhost:8080

# With more options
haskemathesis-cli test \
  --spec openapi.yaml \
  --url http://localhost:8080 \
  --count 200 \
  --tag users \
  --negative \
  --auth-header "Bearer YOUR_TOKEN"
```

Options:

- `-s, --spec FILE` - Path to OpenAPI spec (required)
- `-u, --url URL` - Base URL of the API (required)
- `-n, --count N` - Test cases per operation (default: 100)
- `-i, --include PATTERN` - Include only matching operations (repeatable)
- `-e, --exclude PATTERN` - Exclude matching operations (repeatable)
- `-t, --tag TAG` - Filter by tag (repeatable)
- `--negative` - Enable negative testing (invalid inputs)
- `--auth-header VALUE` - Authorization header value
- `-o, --output FORMAT` - Output format: `text` or `json`
- `--seed INT` - Random seed for reproducibility
- `--timeout SECONDS` - Request timeout
- `-w, --workers N` - Parallel workers (default: 1)
- `--workdir PATH|temp|current` - Working directory (default: `temp`)

#### `validate` - Validate an OpenAPI specification

```bash
haskemathesis-cli validate --spec openapi.yaml
haskemathesis-cli validate --spec openapi.yaml --verbose
```

#### `curl` - Generate curl commands for operations

```bash
haskemathesis-cli curl --spec openapi.yaml --url http://localhost:8080
haskemathesis-cli curl --spec openapi.yaml --url http://localhost:8080 --count 5
```

## Quick Start (Haskell Library)

### WAI

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

### HTTP

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
- `examples/openapi-medium-spec.yaml`

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

- OpenAPI 3.0.x is fully supported via the `openapi3` library.
- OpenAPI 3.1.x is supported with automatic transformation to 3.0 format:
  - Version field is downgraded from `3.1.x` to `3.0.3`
  - Numeric `exclusiveMinimum`/`exclusiveMaximum` are converted to the 3.0 boolean format
  - Other 3.1-specific features (e.g., `null` in type arrays) may require manual adjustment
