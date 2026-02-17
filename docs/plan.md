# Hedgehog OpenAPI — Implementation Plan

**A Haskell library for property-based API testing driven by OpenAPI 3.0/3.1 schemas, powered by Hedgehog.**

Working name: `hedgehog-openapi` (alternatives: `openapi-hedgehog`, `schemahog`)

______________________________________________________________________

## 1. Project Goals & Scope

### What It Does

Given an OpenAPI 3.0 or 3.1 specification, the library:

1. Parses the spec and enumerates all operations (method + path pairs)
1. For each operation, derives Hedgehog generators for valid (and optionally invalid) requests — path params, query params, headers, request bodies
1. Executes generated requests against a handler (either an HTTP endpoint or a WAI `Application` directly)
1. Asserts configurable checks against each response
1. Reports failures as shrunk, minimal counterexamples with full reproduction info

### What It Does NOT Do (v1)

- GraphQL support
- Swagger 2.0 support
- Full stateful/workflow testing (deferred to v2; see § 11)

______________________________________________________________________

## 2. Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                      User Test Suite                         │
│   (Hspec / Tasty / raw Hedgehog / custom)                    │
└──────────────┬───────────────────────────────────────────────┘
               │  uses
┌──────────────▼───────────────────────────────────────────────┐
│                   hedgehog-openapi                            │
│                                                              │
│  ┌─────────────┐  ┌──────────────┐  ┌────────────────────┐  │
│  │ Schema       │  │ Generator    │  │ Check / Assertion  │  │
│  │ Loader &     │─▶│ Engine       │─▶│ Engine             │  │
│  │ Resolver     │  │              │  │                    │  │
│  └─────────────┘  └──────┬───────┘  └────────┬───────────┘  │
│                          │                    │              │
│                  ┌───────▼────────┐  ┌───────▼───────────┐  │
│                  │ Request        │  │ Failure            │  │
│                  │ Executor       │  │ Reporter           │  │
│                  │ (pluggable)    │  │                    │  │
│                  └────────────────┘  └───────────────────┘  │
└──────────────────────────────────────────────────────────────┘
```

The library is structured as five main layers:

| Layer | Responsibility |
|---|---|
| **Schema Loader** | Parse YAML/JSON, resolve `$ref`, normalize to internal IR |
| **Generator Engine** | `Schema → Gen Value`, `Operation → Gen ApiRequest` |
| **Request Executor** | Pluggable: direct WAI `Application` call or `http-client` |
| **Check Engine** | Configurable set of response assertions |
| **Failure Reporter** | Rich, shrunk counterexample rendering |

______________________________________________________________________

## 3. Key Dependencies

| Dependency | Purpose | Notes |
|---|---|---|
| `hedgehog` | Property testing, generators, shrinking | Core |
| `openapi3` (Hackage) | Parse OpenAPI 3.0 specs | Only supports 3.0.x natively |
| `aeson` | JSON value generation & response parsing | |
| `http-types` | Status codes, method types, headers | |
| `http-client` / `http-client-tls` | HTTP transport (optional) | |
| `wai` + `wai-extra` | Direct `Application` testing without network | |
| `text`, `bytestring` | Standard | |
| `containers`, `unordered-containers` | Maps for schemas/params | |
| `scientific` | Numeric JSON values | |
| `lens` / `optics` | Working with `openapi3` types | `openapi3` uses `lens` |
| `yaml` | Loading YAML specs | |
| `uri-encode` | Path/query encoding | |

### OpenAPI 3.1 Strategy

The `openapi3` Hackage package only supports 3.0.x. For 3.1 support:

- **Option A (recommended for v1):** Pre-process 3.1 specs by down-converting the few breaking differences (e.g., `type` as array, `null` as type, `$ref` siblings) into 3.0-compatible form, then feed to `openapi3`.
- **Option B (v2):** Write a lightweight 3.1 parser or fork `openapi3`.

______________________________________________________________________

## 4. Module Structure

```
HedgehogOpenApi
├── Schema
│   ├── Loader          -- Load from file, URL, or Aeson Value
│   ├── Resolve         -- $ref resolution, component inlining
│   └── IR              -- Internal representation (normalized)
├── Gen
│   ├── Schema          -- Schema → Gen Value  (the heart)
│   ├── Param           -- Parameter → Gen (name, value)
│   ├── Request         -- Operation → Gen ApiRequest
│   ├── RequestBody     -- RequestBody → Gen (content-type, body)
│   ├── Negative        -- Intentionally invalid generators
│   └── Format          -- Pluggable format generators (date, email, uuid, uri, etc.)
├── Execute
│   ├── Types           -- ApiRequest, ApiResponse
│   ├── Wai             -- Run against a WAI Application (no network)
│   └── Http            -- Run against a live URL via http-client
├── Check
│   ├── Types           -- Check type, CheckResult
│   ├── Standard        -- Built-in checks (not_a_server_error, response_schema, etc.)
│   └── Custom          -- User-defined check registration
├── Report
│   ├── Failure         -- FailureReport data type
│   ├── Render          -- Pretty-print failures (terminal, structured)
│   └── Curl            -- Generate curl reproduction commands
├── Auth
│   └── Config          -- Auth scheme config (bearer, apikey, basic, custom)
├── Config              -- Top-level library configuration
├── Property            -- High-level: schema → [Property] for test frameworks
└── Integration
    ├── Hspec           -- describe/it wrappers
    └── Tasty           -- testProperty wrappers
```

______________________________________________________________________

## 5. Core Types

```haskell
-- | A fully resolved, generated API request ready for execution.
data ApiRequest = ApiRequest
  { reqMethod      :: !Method           -- GET, POST, etc.
  , reqPath        :: !Text             -- /users/42
  , reqQueryParams :: ![(Text, Text)]
  , reqHeaders     :: ![(HeaderName, ByteString)]
  , reqBody        :: !(Maybe (MediaType, LByteString))
  } deriving (Show, Eq)

-- | The response we got back.
data ApiResponse = ApiResponse
  { resStatusCode  :: !Int
  , resHeaders     :: ![(HeaderName, ByteString)]
  , resBody        :: !LByteString
  , resTime        :: !NominalDiffTime
  } deriving (Show, Eq)

-- | A check is a named predicate on (request, response, operation).
data Check = Check
  { checkName :: !Text
  , checkRun  :: ApiRequest -> ApiResponse -> ResolvedOperation -> CheckResult
  }

data CheckResult
  = CheckPassed
  | CheckFailed !FailureDetail

-- | Rich failure info for reporting.
data FailureDetail = FailureDetail
  { fdCheck       :: !Text
  , fdMessage     :: !Text
  , fdRequest     :: !ApiRequest
  , fdResponse    :: !ApiResponse
  , fdOperation   :: !OperationId
  , fdCurlCommand :: !Text       -- Reproducible curl
  , fdDiff        :: !(Maybe Text) -- Schema diff if applicable
  }

-- | Configuration for a test run.
data TestConfig = TestConfig
  { tcChecks        :: ![Check]
  , tcAuth          :: !(Maybe AuthConfig)
  , tcBaseUrl       :: !(Maybe BaseUrl)     -- Nothing = WAI mode
  , tcPropertyCount :: !Int                 -- default 100
  , tcShrinkLimit   :: !Int
  , tcOperationFilter :: !(OperationId -> Bool)
  , tcNegativeTesting :: !Bool
  , tcFormatRegistry :: !FormatRegistry
  }

-- | Pluggable executor.
class MonadIO m => RequestExecutor m where
  executeRequest :: ApiRequest -> m ApiResponse

-- | Pluggable format generators.
type FormatRegistry = Map Text (Gen Value)
```

______________________________________________________________________

## 6. Generator Engine — The Heart of the Library

This is the most complex component. It must translate arbitrary JSON Schema (as embedded in OpenAPI) into Hedgehog `Gen Value`.

### 6.1 Primitive Generators

```haskell
genFromSchema :: FormatRegistry -> Schema -> Gen Value
genFromSchema fmts schema = case schemaType schema of
  Just "string"  -> genString fmts schema
  Just "integer" -> genInteger schema
  Just "number"  -> genNumber schema
  Just "boolean" -> genBoolean
  Just "array"   -> genArray fmts schema
  Just "object"  -> genObject fmts schema
  Just "null"    -> pure Null
  Nothing        -> genFromConstraints fmts schema  -- infer from other fields
  _              -> genAnyValue  -- fallback

genString :: FormatRegistry -> Schema -> Gen Value
genString fmts s = do
  let minL = fromMaybe 0 (schemaMinLength s)
      maxL = fromMaybe 256 (schemaMaxLength s)
  case schemaFormat s of
    Just fmt | Just gen <- Map.lookup fmt fmts -> gen
    Just "date"      -> genDate
    Just "date-time" -> genDateTime
    Just "email"     -> genEmail
    Just "uuid"      -> genUUID
    Just "uri"       -> genURI
    Just "ipv4"      -> genIPv4
    Just "ipv6"      -> genIPv6
    Just "byte"      -> genBase64 minL maxL
    Just "binary"    -> genBinary minL maxL
    _ -> case schemaEnum s of
      Just vals -> Gen.element vals
      Nothing   -> case schemaPattern s of
        Just pat -> genFromRegex pat  -- best-effort regex gen
        Nothing  -> String <$> Gen.text (Range.linear (fromIntegral minL)
                                                       (fromIntegral maxL))
                                       Gen.unicode

genInteger :: Schema -> Gen Value
genInteger s =
  let lo = maybe (-1000000) ceiling (schemaMinimum s)
      hi = maybe  1000000  floor   (schemaMaximum s)
      -- Handle exclusiveMinimum / exclusiveMaximum
  in Number . fromIntegral <$> Gen.integral (Range.linearFrom 0 lo hi)

genNumber :: Schema -> Gen Value
genNumber s =
  let lo = fromMaybe (-1e6) (schemaMinimum s)
      hi = fromMaybe  1e6   (schemaMaximum s)
  in Number . realToFrac <$> Gen.double (Range.linearFracFrom 0 lo hi)
```

### 6.2 Composite Schema Generators

```haskell
-- allOf: generate a value satisfying all sub-schemas (merge objects)
genAllOf :: FormatRegistry -> [Schema] -> Gen Value
genAllOf fmts schemas = do
  vals <- traverse (genFromSchema fmts) schemas
  pure $ mergeObjects vals

-- oneOf / anyOf: pick one sub-schema and generate from it
genOneOf :: FormatRegistry -> [Schema] -> Gen Value
genOneOf fmts schemas =
  Gen.choice (map (genFromSchema fmts) schemas)

-- Handle discriminator by weighting/selecting based on the discriminator property
genWithDiscriminator :: FormatRegistry -> Discriminator -> [Schema] -> Gen Value
genWithDiscriminator fmts disc schemas = -- ...

-- Objects: generate required + optional properties
genObject :: FormatRegistry -> Schema -> Gen Value
genObject fmts s = do
  let required = Set.fromList (fromMaybe [] (schemaRequired s))
      props    = Map.toList (fromMaybe mempty (schemaProperties s))
  reqFields <- for [(k, v) | (k, v) <- props, k `Set.member` required] $ \(k, v) ->
    (k,) <$> genFromSchema fmts v
  optFields <- for [(k, v) | (k, v) <- props, k `Set.notMember` required] $ \(k, v) ->
    Gen.maybe (genFromSchema fmts v) >>= \case
      Nothing  -> pure Nothing
      Just val -> pure (Just (k, val))
  let addlProps = case schemaAdditionalProperties s of
        Just (AdditionalPropertiesSchema addlSchema) ->
          Gen.list (Range.linear 0 3) $ do
            key <- Gen.text (Range.linear 1 10) Gen.alphaNum
            val <- genFromSchema fmts addlSchema
            pure (key, val)
        _ -> pure []
  extras <- addlProps
  pure . Object . KeyMap.fromList $
    reqFields <> catMaybes optFields <> extras

-- Arrays
genArray :: FormatRegistry -> Schema -> Gen Value
genArray fmts s = do
  let minI = fromMaybe 0 (schemaMinItems s)
      maxI = fromMaybe 10 (schemaMaxItems s)
      itemSchema = fromMaybe (mempty) (schemaItems s)
  items <- Gen.list (Range.linear (fromIntegral minI) (fromIntegral maxI))
                    (genFromSchema fmts itemSchema)
  -- Handle uniqueItems
  let items' = if fromMaybe False (schemaUniqueItems s)
               then nub items
               else items
  pure (Array (Vector.fromList items'))
```

### 6.3 Recursive Schema Handling

Recursive schemas (e.g., a `TreeNode` with `children: [TreeNode]`) require bounded recursion:

```haskell
genFromSchemaWithDepth :: FormatRegistry -> Int -> Schema -> Gen Value
genFromSchemaWithDepth fmts depth schema
  | depth <= 0 = genLeafFallback schema  -- generate minimal valid leaf
  | otherwise  = -- normal generation with (depth - 1) for nested calls
```

Use `Gen.recursive` from Hedgehog where appropriate, or a manual fuel parameter.

### 6.4 Request-Level Generation

```haskell
genApiRequest :: FormatRegistry -> ResolvedOperation -> Gen ApiRequest
genApiRequest fmts op = do
  pathParams  <- traverse (genParam fmts) (opPathParams op)
  queryParams <- traverse (genParam fmts) (opQueryParams op)
  headers     <- traverse (genParam fmts) (opHeaderParams op)
  body        <- traverse (genRequestBody fmts) (opRequestBody op)
  let path = interpolatePath (opPathTemplate op) pathParams
  pure ApiRequest
    { reqMethod      = opMethod op
    , reqPath        = path
    , reqQueryParams = [(pName p, pValue p) | p <- queryParams]
    , reqHeaders     = [(CI.mk (encodeUtf8 (pName p)), encodeUtf8 (pValue p))
                       | p <- headers]
    , reqBody        = body
    }
```

______________________________________________________________________

## 7. Negative Testing (Invalid Input Generation)

Negative testing generates intentionally schema-violating requests to ensure the API rejects them properly (expects 4xx, not 2xx or 5xx).

### Strategy: Mutation-Based

Given a valid generated value, apply one or more mutations:

```haskell
data Mutation
  = WrongType          -- string where integer expected, etc.
  | MissingRequired    -- drop a required field
  | OutOfRange         -- exceed min/max
  | ExtraProperty      -- add unknown property when additionalProperties: false
  | InvalidFormat      -- "not-a-date" for format: date
  | NullNonNullable    -- null for a non-nullable field
  | InvalidEnum        -- value not in enum list
  | ArrayTooLong       -- exceed maxItems
  | ArrayTooShort      -- below minItems

genNegativeRequest :: FormatRegistry -> ResolvedOperation -> Gen (ApiRequest, Mutation)
genNegativeRequest fmts op = do
  validReq <- genApiRequest fmts op
  mutation <- Gen.element applicableMutations
  mutatedReq <- applyMutation mutation validReq
  pure (mutatedReq, mutation)
```

______________________________________________________________________

## 8. Built-in Checks

Modeled after Schemathesis's check system:

| Check | What it validates |
|---|---|
| `notAServerError` | Response status ≠ 5xx |
| `responseSchemaConformance` | Response body validates against the response schema for the returned status code |
| `statusCodeConformance` | Returned status code is documented in the spec |
| `contentTypeConformance` | Response `Content-Type` matches documented media types |
| `responseHeadersConformance` | Required response headers are present and valid |
| `negativeTestRejection` | For negative tests: expects 4xx, not 2xx |
| `requestAcceptance` | For valid requests: expects 2xx (with allowances for 401/403/404/409) |

```haskell
-- The default check set
defaultChecks :: [Check]
defaultChecks =
  [ notAServerError
  , responseSchemaConformance
  , statusCodeConformance
  ]

allChecks :: [Check]
allChecks = defaultChecks ++
  [ contentTypeConformance
  , responseHeadersConformance
  ]
```

### Response Schema Validation

```haskell
responseSchemaConformance :: Check
responseSchemaConformance = Check "response_schema_conformance" $ \req res op ->
  case findResponseSchema (resStatusCode res) op of
    Nothing     -> CheckPassed  -- no schema defined, can't validate
    Just schema ->
      case Aeson.eitherDecode (resBody res) of
        Left err  -> CheckFailed (detail "Response body is not valid JSON" ...)
        Right val ->
          case validateJSON schema val of
            []   -> CheckPassed
            errs -> CheckFailed (detail (renderValidationErrors errs) ...)
```

______________________________________________________________________

## 9. Failure Reporting

### 9.1 FailureReport Structure

Each failure captures everything needed for reproduction:

```haskell
data FailureReport = FailureReport
  { frOperationId  :: !Text           -- "GET /users/{id}"
  , frCheckName    :: !Text           -- "response_schema_conformance"
  , frMessage      :: !Text           -- Human-readable explanation
  , frRequest      :: !ApiRequest     -- The (shrunk) failing request
  , frResponse     :: !ApiResponse    -- What came back
  , frCurl         :: !Text           -- curl -X GET ...
  , frSeed         :: !Seed           -- Hedgehog seed for replay
  , frSchemaDiff   :: !(Maybe Text)   -- For schema violations: expected vs actual
  , frMutation     :: !(Maybe Mutation) -- For negative tests
  }
```

### 9.2 Curl Command Generation

```haskell
toCurl :: ApiRequest -> Maybe BaseUrl -> Text
toCurl req mBase =
  T.unwords $ filter (not . T.null)
    [ "curl"
    , "-X", T.pack (show (reqMethod req))
    , headerFlags
    , bodyFlag
    , quote (baseUrl <> reqPath req <> queryString)
    ]
  where
    headerFlags = T.unwords
      ["-H " <> quote (decodeUtf8 n <> ": " <> decodeUtf8 v)
      | (n, v) <- reqHeaders req]
    bodyFlag = case reqBody req of
      Nothing -> ""
      Just (_, b) -> "-d " <> quote (decodeUtf8 (BSL.toStrict b))
    queryString
      | null (reqQueryParams req) = ""
      | otherwise = "?" <> T.intercalate "&"
          [k <> "=" <> v | (k, v) <- reqQueryParams req]
```

### 9.3 Rendering

Terminal output for a failure looks like:

```
━━━ hedgehog-openapi ━━━

  ✗ POST /bookings — response_schema_conformance

    Response violates schema:
      'room_type' is a required property
      Expected: {"required": ["room_type", "guest_name"], "type": "object"}

    Shrunk request (after 12 shrinks):
      POST /bookings
      Content-Type: application/json
      Authorization: Bearer <redacted>
      {"guest_name": "", "nights": 1}

    Response: 200 OK (12ms)
      {"id": 1, "status": "confirmed"}

    Reproduce with:
      curl -X POST -H 'Content-Type: application/json' \
        -H 'Authorization: Bearer <token>' \
        -d '{"guest_name": "", "nights": 1}' \
        http://localhost:8080/bookings

    Hedgehog seed: 12345678901234/67890
```

______________________________________________________________________

## 10. Test Framework Integration

### 10.1 High-Level API — The Primary User Interface

```haskell
-- | Generate Hedgehog properties for all operations in a spec.
propertiesForSpec
  :: TestConfig
  -> OpenApi                       -- parsed spec
  -> (ApiRequest -> IO ApiResponse)  -- executor function
  -> [(PropertyName, Property)]

-- | Generate a property for a single operation.
propertyForOperation
  :: TestConfig
  -> ResolvedOperation
  -> (ApiRequest -> IO ApiResponse)
  -> Property
propertyForOperation cfg op execute = property $ do
  req <- forAll (genApiRequest (tcFormatRegistry cfg) op)
  let req' = applyAuth (tcAuth cfg) req
  res <- evalIO (execute req')
  for_ (tcChecks cfg) $ \check ->
    case checkRun check req' res op of
      CheckPassed   -> success
      CheckFailed d -> do
        annotate (renderFailureDetail d)
        failure
```

### 10.2 Hspec Integration

```haskell
-- | Run all operations against a WAI Application in Hspec.
specForApp :: TestConfig -> OpenApi -> Application -> Spec
specForApp cfg spec app = do
  let ops = resolveOperations spec
  describe "OpenAPI Conformance" $
    forM_ ops $ \op ->
      it (T.unpack (operationLabel op)) $
        hedgehog (propertyForOperation cfg op (executeWai app))

-- User's test file:
main :: IO ()
main = hspec $ do
  let cfg = defaultConfig { tcAuth = Just (bearerToken "secret") }
  spec <- loadSpec "openapi.yaml"
  specForApp cfg spec myWaiApp
```

### 10.3 Tasty Integration

```haskell
testTreeForSpec :: TestConfig -> OpenApi -> (ApiRequest -> IO ApiResponse) -> TestTree
testTreeForSpec cfg spec execute =
  testGroup "OpenAPI Conformance"
    [ testPropertyNamed (operationLabel op) (operationLabel op)
        (propertyForOperation cfg op execute)
    | op <- resolveOperations spec
    ]
```

### 10.4 Direct WAI Testing (No HTTP)

```haskell
executeWai :: Application -> ApiRequest -> IO ApiResponse
executeWai app req = do
  let waiReq = toWaiRequest req
  (status, headers, body) <- receivedResponse <$> runSessionWith app waiReq
  pure ApiResponse
    { resStatusCode = statusCode status
    , resHeaders    = headers
    , resBody       = body
    , resTime       = 0  -- or measure with bracket
    }
```

This is the **recommended default** — no network, no port management, fast feedback.

______________________________________________________________________

## 11. Authentication Support

```haskell
data AuthConfig
  = BearerToken !Text
  | BasicAuth !Text !Text          -- username, password
  | ApiKeyHeader !Text !Text       -- header name, value
  | ApiKeyQuery !Text !Text        -- param name, value
  | CustomAuth !(ApiRequest -> ApiRequest)  -- arbitrary transform

applyAuth :: Maybe AuthConfig -> ApiRequest -> ApiRequest
applyAuth Nothing req = req
applyAuth (Just (BearerToken t)) req =
  req { reqHeaders = ("Authorization", "Bearer " <> encodeUtf8 t) : reqHeaders req }
applyAuth (Just (ApiKeyHeader name val)) req =
  req { reqHeaders = (CI.mk (encodeUtf8 name), encodeUtf8 val) : reqHeaders req }
-- etc.
```

The library should also read security schemes from the OpenAPI spec and auto-apply them when `AuthConfig` is provided.

______________________________________________________________________

## 12. Implementation Phases

### Phase 1: Core Generator Engine (Weeks 1–3)

**Goal:** `Schema → Gen Value` for all JSON Schema types.

- [ ] Set up cabal project, CI, basic module structure
- [ ] Implement `genFromSchema` for primitives: string, integer, number, boolean, null
- [ ] Handle `enum`, `const`
- [ ] Handle `minLength`/`maxLength`, `minimum`/`maximum`, `exclusiveMin/Max`
- [ ] Handle `pattern` (basic regex → generator, or fallback)
- [ ] Implement `genObject` with required/optional/additionalProperties
- [ ] Implement `genArray` with minItems/maxItems/uniqueItems
- [ ] Implement `allOf`, `oneOf`, `anyOf` composition
- [ ] Handle `nullable`
- [ ] Built-in format generators: date, date-time, email, uuid, uri, ipv4, ipv6, byte
- [ ] Pluggable `FormatRegistry` for custom formats
- [ ] Recursive schema support with depth limiting
- [ ] Comprehensive test suite: round-trip tests (generate → validate against schema)

**Deliverable:** A standalone `genFromSchema :: Schema -> Gen Value` that works.

### Phase 2: Request Generation & Execution (Weeks 4–5)

**Goal:** `Operation → Gen ApiRequest → ApiResponse`

- [ ] Schema loader: parse from file path, `Value`, or URL
- [ ] `$ref` resolution across components
- [ ] OpenAPI 3.1 → 3.0 pre-processing shim
- [ ] Parameter generation (path, query, header, cookie)
- [ ] Path template interpolation
- [ ] Request body generation (JSON focus; `multipart/form-data` stretch goal)
- [ ] `ApiRequest` / `ApiResponse` types
- [ ] WAI executor (direct `Application` testing)
- [ ] HTTP executor (via `http-client`)
- [ ] Operation enumeration from spec

**Deliverable:** Can generate and execute requests for all operations in a spec.

### Phase 3: Check Engine & Reporting (Weeks 6–7)

**Goal:** Detect and report failures beautifully.

- [ ] `Check` type and `CheckResult`
- [ ] Implement `notAServerError`
- [ ] Implement `responseSchemaConformance` (JSON Schema validation of responses)
- [ ] Implement `statusCodeConformance`
- [ ] Implement `contentTypeConformance`
- [ ] Curl command generation from `ApiRequest`
- [ ] `FailureReport` pretty-printing with ANSI colors
- [ ] Schema diff rendering for schema violations
- [ ] Seed capture for Hedgehog replay

**Deliverable:** Failing tests produce actionable, shrunk counterexamples.

### Phase 4: Test Framework Integration & Auth (Weeks 8–9)

**Goal:** Plug-and-play for Hspec, Tasty, and raw Hedgehog users.

- [ ] `propertiesForSpec` high-level API
- [ ] Hspec integration module (`specForApp`, `specForUrl`)
- [ ] Tasty integration module
- [ ] Auth configuration (Bearer, Basic, API key, custom)
- [ ] Auto-detect security schemes from spec
- [ ] Operation filtering (by path, tag, operationId)
- [ ] `TestConfig` with sensible defaults

**Deliverable:** A user can write a 5-line test file and get full API conformance testing.

### Phase 5: Negative Testing (Week 10)

**Goal:** Generate intentionally invalid requests and verify rejection.

- [ ] Mutation types (wrong type, missing required, out of range, etc.)
- [ ] `genNegativeRequest` that applies a single mutation to a valid request
- [ ] `negativeTestRejection` check (expect 4xx)
- [ ] Annotation of which mutation was applied in failure reports
- [ ] User toggle: `tcNegativeTesting :: Bool`

**Deliverable:** Library can test both happy path and error handling.

### Phase 6: Polish & Release (Weeks 11–12)

- [ ] Haddock documentation for all public modules
- [ ] README with quick-start guide
- [ ] Example project (test a sample WAI app)
- [ ] Property tests for the generators themselves (meta-testing)
- [ ] Performance profiling for large specs (100+ operations)
- [ ] Hackage release preparation
- [ ] Changelog, license (MIT or BSD-3), contribution guide

______________________________________________________________________

## 13. Future Work (v2+)

### Stateful / Workflow Testing

Use Hedgehog's state machine testing to chain operations:

- `POST /users` → extract `id` → `GET /users/{id}` → `DELETE /users/{id}`
- Model this using OpenAPI `links` or user-defined operation chains
- Leverage `Hedgehog.Internal.State` for state machine properties

### Additional Features

- **OpenAPI 3.1 native parser** (full JSON Schema 2020-12 support)
- **Request/response logging** (VCR-style cassette recording)
- **Coverage tracking** (which operations/status codes were exercised)
- **Targeted property testing** (use `Hedgehog.Internal.Opaque` to guide generation toward interesting responses, e.g., maximize response time or body size)
- **CLI tool** wrapping the library for quick one-off runs
- **Parallel operation testing** (concurrent requests to find race conditions)

______________________________________________________________________

## 14. Design Decisions & Rationale

### Why Hedgehog over QuickCheck?

- **Integrated shrinking**: Generators and shrinking are unified, so shrunk counterexamples always satisfy the schema constraints. This is critical — a shrunk request that violates the schema is useless noise.
- **Monadic generators**: Hedgehog generators are monadic (`GenT m`), which naturally fits the conditional/dependent generation needed for OpenAPI (e.g., generate a path param, then use it in the path).
- **Better reporting**: Built-in diffs, annotations, and labeling.

### Why WAI-first execution?

- No port management, no race conditions, no network flakiness
- Faster feedback loop (microseconds per request vs milliseconds)
- Users who need HTTP can opt in via `executeHttp`
- Matches the "library for test suites" use case

### Why pluggable executor?

Some users will want to test against a live staging server. Others will want to test a WAI `Application` directly. A few might want to test Servant handlers via `servant-client`. The `ApiRequest -> IO ApiResponse` function type makes this completely flexible without needing a typeclass.

### $ref Resolution Strategy

Resolve all `$ref`s eagerly at load time into a flat map of schemas. This avoids threading reference resolution through every generator and simplifies the recursive schema handling.

______________________________________________________________________

## 15. Risks & Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| `openapi3` package doesn't cover all 3.0 edge cases | Generator gaps | Contribute upstream; maintain a thin compatibility layer |
| OpenAPI 3.1's JSON Schema alignment is complex | 3.1 support incomplete | Start with 3.0; 3.1 shim handles 80% of cases |
| Regex-to-generator is hard | `pattern` fields produce poor values | Use `genregex` or similar; fall back to unconstrained string |
| Large specs (1000+ operations) may be slow | Test suite takes too long | Operation filtering; user controls property count per op |
| Hedgehog shrinking may be slow for deeply nested JSON | Slow feedback | Limit shrink depth; set reasonable `ShrinkLimit` |
| Some APIs need cookies/sessions/CSRF tokens | Can't test authenticated endpoints | `CustomAuth` escape hatch; document limitation |
