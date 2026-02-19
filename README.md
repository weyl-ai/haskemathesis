# `haskemathesis`

Property-based API testing driven by OpenAPI 3.x schemas, powered by Hedgehog, inspired by python's [`schemathesis`](https://github.com/schemathesis/schemathesis).

This library loads an OpenAPI spec, generates valid requests from the schema,
executes them against either a WAI `Application` or a live HTTP endpoint, and
applies a configurable set of checks to the responses.

## Features

- OpenAPI 3.0 and 3.1 schema parsing and operation resolution
- Schema-driven generators for JSON requests
- WAI executor (no network) and HTTP executor
- Built-in checks (status codes, content type, response schema, response headers, response time)
- Negative testing (invalid inputs that should be rejected)
- Stateful testing (CRUD sequences with automatic link inference)
- Curl rendering for failure reproduction
- Hspec and Tasty integration helpers
- Standalone CLI for testing any HTTP API without writing Haskell
- Load specs from file paths or URLs
- JUnit XML output for CI integration (Jenkins, GitLab CI, GitHub Actions)

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

- `-s, --spec FILE|URL` - Path or URL to OpenAPI spec (required)
- `-u, --url URL` - Base URL of the API (required)
- `-n, --count N` - Test cases per operation (default: 100)
- `-i, --include PATTERN` - Include only matching operations (repeatable)
- `-e, --exclude PATTERN` - Exclude matching operations (repeatable)
- `-t, --tag TAG` - Filter by tag (repeatable)
- `--negative` - Enable negative testing (invalid inputs)
- `--stateful` - Enable stateful testing (CRUD sequences)
- `--max-sequence-length N` - Max operations per stateful sequence (default: 5)
- `--auth-header VALUE` - Authorization header value
- `-o, --output FORMAT` - Output format: `text`, `json`, or `junit`
- `--max-response-time MS` - Fail if response time exceeds threshold (ms)
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

### Loading Specs from URLs

The CLI can load OpenAPI specifications directly from HTTP/HTTPS URLs:

```bash
# Load spec from Swagger Petstore
haskemathesis-cli test \
  --spec https://petstore.swagger.io/v2/swagger.json \
  --url http://localhost:8080

# Load from private endpoint with auth
haskemathesis-cli test \
  --spec https://api.example.com/docs/openapi.yaml \
  --url https://api.example.com
```

### JUnit XML Output

Generate JUnit XML reports for CI integration:

```bash
# Output JUnit XML to stdout
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --output junit

# Save to file for CI artifacts
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --output junit > test-results.xml
```

Example CI configurations:

**GitHub Actions:**

```yaml
- run: haskemathesis-cli test --spec api.yaml --url $API_URL --output junit > results.xml
- uses: dorny/test-reporter@v1
  with:
    name: API Tests
    path: results.xml
    reporter: java-junit
```

**GitLab CI:**

```yaml
test:
  script:
    - haskemathesis-cli test --spec api.yaml --url $API_URL --output junit > results.xml
  artifacts:
    reports:
      junit: results.xml
```

### Response Time Validation

Fail tests if API response times exceed a threshold:

```bash
# Fail if any response takes longer than 500ms
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --max-response-time 500

# Strict SLA: 100ms limit
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --max-response-time 100
```

## Quick Start (Haskell Library)

### Loading OpenAPI Specs

Load specs from files or URLs using `loadOpenApi`:

```haskell
import Haskemathesis.OpenApi.Loader (loadOpenApi, loadOpenApiFile, loadOpenApiUrl)

-- Auto-detect file path or URL
spec1 <- loadOpenApi "openapi.yaml"
spec2 <- loadOpenApi "https://api.example.com/openapi.json"

-- Explicitly load from file
spec3 <- loadOpenApiFile "/path/to/api.yaml"

-- Explicitly load from URL
spec4 <- loadOpenApiUrl "https://petstore.swagger.io/v2/swagger.json"
```

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

## Stateful Testing

Stateful testing goes beyond individual endpoint testing by generating sequences
of related API operations. This is essential for testing CRUD workflows where
operations depend on each other (e.g., you must create a resource before you can
read, update, or delete it).

### CLI Usage

```bash
# Enable stateful testing
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --stateful

# Control sequence length (default: 5)
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --stateful --max-sequence-length 10

# Combine with other options
haskemathesis-cli test \
  --spec api.yaml \
  --url http://localhost:8080 \
  --stateful \
  --tag users \
  --auth-header "Bearer TOKEN"
```

### How It Works

Stateful testing automatically:

1. **Identifies CRUD operations** - Finds POST (create), GET (read), PUT/PATCH
   (update), and DELETE operations on the same resource path.

1. **Infers operation links** - Detects that `POST /users` returns an `id` that
   should be used as the `{id}` parameter in `GET /users/{id}`.

1. **Generates operation sequences** - Creates realistic test sequences like:

   ```
   POST /users → GET /users/{id} → PUT /users/{id} → DELETE /users/{id}
   ```

1. **Tracks state across requests** - Extracts IDs and other values from
   responses and uses them in subsequent requests.

1. **Runs stateful checks** - Verifies invariants like:

   - **Use-after-free**: Accessing a deleted resource should return 404
   - **Resource availability**: Created resources should be retrievable
   - **Modification persistence**: Updated data should be reflected in GET

### Haskell Library Usage

```haskell
import Haskemathesis.Config (defaultTestConfig, defaultStatefulChecks)
import Haskemathesis.Property (propertiesForSpecStateful)

let config = defaultTestConfig
      { tcStatefulTesting = True
      , tcStatefulChecks = defaultStatefulChecks
      , tcMaxSequenceLength = 5
      , tcCleanupOnFailure = True
      }
```

### OpenAPI Link Support

Haskemathesis supports explicit [OpenAPI Links](https://swagger.io/docs/specification/links/)
for defining relationships between operations:

```yaml
paths:
  /users:
    post:
      operationId: createUser
      responses:
        '201':
          description: User created
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'
          links:
            GetUser:
              operationId: getUser
              parameters:
                id: '$response.body#/id'
  /users/{id}:
    get:
      operationId: getUser
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
```

When explicit links are defined, they take precedence over heuristic inference.

### Heuristic Link Inference

Even without explicit OpenAPI links, Haskemathesis infers relationships using:

- **Path patterns**: `POST /users` links to `/users/{id}` operations
- **Field name matching**: Response field `userId` matches path param `{userId}`
- **Common ID conventions**: Fields named `id`, `*Id`, `*_id` are extracted

### Configuration Options

| Option | CLI Flag | Default | Description |
|--------|----------|---------|-------------|
| `tcStatefulTesting` | `--stateful` | `False` | Enable stateful testing |
| `tcMaxSequenceLength` | `--max-sequence-length` | `5` | Max operations per sequence |
| `tcStatefulChecks` | - | `defaultStatefulChecks` | Stateful invariant checks |
| `tcCleanupOnFailure` | - | `True` | Run cleanup (DELETE) on failure |

### Stateful Checks

The default stateful checks (`defaultStatefulChecks`) include:

- **use-after-free**: After DELETE, GET should return 404
- **resource-availability**: After POST, GET should return 200

Additional checks can be added:

- **modification-persisted**: After PUT/PATCH, GET should reflect changes

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

### Response Time Check

Add a response time limit to your checks:

```haskell
import Haskemathesis.Check.Standard (defaultChecks)
import Haskemathesis.Check.Standard.ResponseTime (maxResponseTime)

-- Fail if any response exceeds 500ms
checks = defaultChecks ++ [maxResponseTime 500]

-- Strict SLA: 100ms limit
strictChecks = defaultChecks ++ [maxResponseTime 100]
```

## Streaming Endpoints

WAI-based tests (in-memory, synchronous) cannot handle streaming endpoints
like Server-Sent Events (SSE) or NDJSON streams. These operations are
automatically filtered out when using WAI executor functions.

To see which operations were skipped, use the `*IO` variants:

```haskell
import Haskemathesis.Integration.Tasty (testTreeForAppIO)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    spec <- loadOpenApiFile "api.yaml"
    (tests, skipped) <- testTreeForAppIO defaultTestConfig spec myApp
    unless (null skipped) $
        hPutStrLn stderr $ "Skipped streaming operations: " ++ show skipped
    defaultMain tests
```

Streaming endpoints are detected by their content types:

- `text/event-stream` (Server-Sent Events)
- `application/x-ndjson` (Newline-delimited JSON)
- `application/stream+json`

For streaming endpoints, use `testTreeForUrl` with a running HTTP server
which properly handles timeouts.

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
