# Stateful Testing Implementation Plan for Haskemathesis

## Overview

Stateful testing chains API operations together, using responses from earlier operations to inform requests of later operations. For example:

```
POST /users → creates user, returns {id: 123}
GET /users/123 → uses id from POST response
PUT /users/123 → updates using same id
DELETE /users/123 → removes the resource
GET /users/123 → should return 404 (use-after-free check)
```

This is a key differentiator for Schemathesis and would make Haskemathesis competitive for real-world API testing.

## Key Design Decisions

### 1. State Representation

```haskell
-- The state accumulated during a stateful test
data TestState = TestState
    { tsResponses :: [(ResolvedOperation, ApiRequest, ApiResponse)]
    -- ^ History of all operations executed
    , tsExtractedValues :: Map Text Value
    -- ^ Named values extracted from responses (e.g., "userId" -> Number 123)
    , tsCreatedResources :: [ResourceRef]
    -- ^ Resources created that should be cleaned up / checked
    }

data ResourceRef = ResourceRef
    { rrOperation :: ResolvedOperation  -- The POST/PUT that created it
    , rrIdentifier :: Value             -- The ID extracted
    , rrPath :: Text                    -- e.g., "/users/{id}"
    }
```

### 2. Link Detection Strategy

Three ways to detect operation relationships:

#### A. OpenAPI Links (Explicit)

The OpenAPI spec can define explicit links:

```yaml
paths:
  /users:
    post:
      responses:
        '201':
          links:
            GetUser:
              operationId: getUser
              parameters:
                id: '$response.body#/id'
```

#### B. Path Pattern Matching (Heuristic)

Detect relationships by matching paths:

- `POST /users` → `GET /users/{id}` (same base path, id param)
- `POST /items` → `PUT /items/{itemId}` → `DELETE /items/{itemId}`

#### C. Response Schema Analysis (Heuristic)

Match response field names to parameter names:

- POST returns `{"id": 123}` → operation has `{id}` path param

### 3. Value Extraction

Extract values from responses based on:

- JSON path expressions (`$response.body#/id`)
- Parameter name matching (response `id` → path param `id`)
- Header extraction (`Location` header → resource URL)

______________________________________________________________________

## Module Structure

```
src/Haskemathesis/
├── Stateful/
│   ├── Types.hs          -- TestState, ResourceRef, Link types
│   ├── Links.hs          -- Parse OpenAPI Links, detect relationships
│   ├── Extract.hs        -- Extract values from responses
│   ├── Sequence.hs       -- Generate operation sequences
│   ├── Generator.hs      -- State-aware request generation
│   └── Checks.hs         -- Stateful checks (use-after-free, etc.)
├── Property.hs           -- Add stateful property functions
└── Config.hs             -- Add stateful config options
```

______________________________________________________________________

## Implementation Phases

### Phase 1: Foundation Types and State Management

**Files to create:**

- `src/Haskemathesis/Stateful/Types.hs`

**Types:**

```haskell
-- | State accumulated during stateful testing
data TestState = TestState
    { tsExtractedValues :: Map Text Value
    , tsCreatedResources :: [ResourceRef]
    , tsHistory :: [(Text, ApiRequest, ApiResponse)]  -- operationId/label
    }

-- | Reference to a created resource
data ResourceRef = ResourceRef
    { rrOperationId :: Text           -- Operation that created it
    , rrResourcePath :: Text          -- Path template, e.g., "/users/{id}"
    , rrIdentifiers :: Map Text Value -- Extracted IDs, e.g., {"id": 123}
    }

-- | A link between operations
data OperationLink = OperationLink
    { olSourceOp :: Text              -- Source operation ID
    , olTargetOp :: Text              -- Target operation ID
    , olParamBindings :: Map Text ValueSource
    }

-- | Where a parameter value comes from
data ValueSource
    = FromResponseBody JsonPath       -- e.g., "$.id" or "$response.body#/id"
    | FromResponseHeader Text         -- e.g., "Location"
    | FromState Text                  -- Named value in TestState
    | Literal Value                   -- Static value
```

### Phase 2: OpenAPI Links Parsing

**Files to create:**

- `src/Haskemathesis/Stateful/Links.hs`

**Functions:**

```haskell
-- | Extract links from OpenAPI spec
extractLinks :: OpenApi -> [OperationLink]

-- | Parse OpenAPI expression (e.g., "$response.body#/id")
parseExpression :: Text -> Either Text ValueSource

-- | Find target operation by operationId or operationRef
resolveTarget :: OpenApi -> Link -> Maybe ResolvedOperation
```

### Phase 3: Heuristic Link Detection

**Add to Links.hs:**

```haskell
-- | Detect links by path pattern matching
inferLinksFromPaths :: [ResolvedOperation] -> [OperationLink]

-- | Match response schema fields to path parameters
inferLinksFromSchemas :: [ResolvedOperation] -> [OperationLink]

-- | Combine explicit and inferred links
allLinks :: OpenApi -> [ResolvedOperation] -> [OperationLink]
```

**Heuristics:**

1. `POST /resources` links to `GET/PUT/DELETE /resources/{id}`
1. Response body field `id` matches path param `id`
1. Response body field `resourceId` matches path param `resource_id` (case normalization)

### Phase 4: Value Extraction

**Files to create:**

- `src/Haskemathesis/Stateful/Extract.hs`

**Functions:**

```haskell
-- | Extract a value from a response based on ValueSource
extractValue :: ValueSource -> ApiResponse -> Maybe Value

-- | Extract all matching values from response body
extractByFieldName :: Text -> Value -> Maybe Value

-- | Parse JSON path and extract value
extractByJsonPath :: Text -> ByteString -> Maybe Value

-- | Extract resource ID from Location header
extractFromLocation :: ApiResponse -> Maybe Text
```

### Phase 5: State-Aware Request Generation

**Files to create:**

- `src/Haskemathesis/Stateful/Generator.hs`

**Functions:**

```haskell
-- | Generate a request using state for parameter values
genStatefulRequest :: TestState -> ResolvedOperation -> Gen ApiRequest

-- | Try to fill parameters from state, fall back to schema generation
fillFromState :: TestState -> ResolvedParam -> Gen (Maybe Value)

-- | Update state after a successful request
updateState :: ResolvedOperation -> ApiRequest -> ApiResponse -> TestState -> TestState
```

### Phase 6: Operation Sequencing

**Files to create:**

- `src/Haskemathesis/Stateful/Sequence.hs`

**Types and Functions:**

```haskell
-- | A sequence of operations to execute
data OperationSequence = OperationSequence
    { osSteps :: [SequenceStep]
    , osCleanup :: [ResolvedOperation]  -- DELETE operations for cleanup
    }

data SequenceStep = SequenceStep
    { ssOperation :: ResolvedOperation
    , ssParamSources :: Map Text ValueSource  -- How to fill each param
    }

-- | Generate valid operation sequences from links
genOperationSequence :: [ResolvedOperation] -> [OperationLink] -> Gen OperationSequence

-- | Common patterns: CRUD sequence, read-modify-write, etc.
genCrudSequence :: ResolvedOperation -> [ResolvedOperation] -> Gen OperationSequence
```

### Phase 7: Stateful Checks

**Files to create:**

- `src/Haskemathesis/Stateful/Checks.hs`

**Checks:**

```haskell
-- | Check: deleted resources return 404
useAfterFree :: StatefulCheck
useAfterFree state req res op =
    -- After DELETE, GET on same resource should return 404
    ...

-- | Check: created resources are immediately accessible
ensureResourceAvailability :: StatefulCheck
ensureResourceAvailability state req res op =
    -- After POST returns 201, GET on that resource should return 200
    ...

-- | Check: modifications are persisted
ensureModificationPersisted :: StatefulCheck
ensureModificationPersisted state req res op =
    -- After PUT, GET should return the modified data
    ...
```

### Phase 8: Stateful Property Functions

**Update:** `src/Haskemathesis/Property.hs`

**Add:**

```haskell
-- | Run a stateful test sequence
propertyStateful :: 
    OpenApi -> 
    TestConfig -> 
    ExecutorWithTimeout -> 
    [ResolvedOperation] -> 
    Property
propertyStateful openApi config execute ops = property $ do
    let links = allLinks openApi ops
    sequence <- forAll (genOperationSequence ops links)
    finalState <- foldM (executeStep execute config) emptyState (osSteps sequence)
    -- Run cleanup and stateful checks
    runStatefulChecks config finalState

-- | Execute a single step in a stateful sequence
executeStep ::
    ExecutorWithTimeout ->
    TestConfig ->
    TestState ->
    SequenceStep ->
    PropertyT IO TestState
executeStep execute config state step = do
    req <- forAll (genStatefulRequest state (ssOperation step))
    res <- evalIO (execute Nothing req)
    runChecks ... -- Regular checks
    pure (updateState (ssOperation step) req res state)
```

### Phase 9: Config Extensions

**Update:** `src/Haskemathesis/Config.hs`

```haskell
data TestConfig = TestConfig
    { ...existing fields...
    , tcStatefulTesting :: Bool           -- Enable stateful testing
    , tcStatefulChecks :: [StatefulCheck] -- Which stateful checks to run
    , tcMaxSequenceLength :: Int          -- Max operations per sequence (default: 5)
    , tcCleanupOnFailure :: Bool          -- Run DELETE cleanup even on failure
    }
```

### Phase 10: CLI Integration

**Update:** `src/Haskemathesis/CLI/Options.hs`

```haskell
data TestOptions = TestOptions
    { ...existing fields...
    , testStateful :: Bool                -- --stateful flag
    , testMaxSequenceLength :: Maybe Int  -- --max-sequence-length
    }
```

______________________________________________________________________

## Example Usage

### OpenAPI with Links

```yaml
paths:
  /users:
    post:
      operationId: createUser
      responses:
        '201':
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'
          links:
            GetUser:
              operationId: getUser
              parameters:
                id: '$response.body#/id'
            DeleteUser:
              operationId: deleteUser
              parameters:
                id: '$response.body#/id'
  /users/{id}:
    get:
      operationId: getUser
      parameters:
        - name: id
          in: path
    delete:
      operationId: deleteUser
      parameters:
        - name: id
          in: path
```

### Generated Test Sequence

```
1. POST /users with body {"name": "Alice"}
   → Response: {"id": 42, "name": "Alice"}
   → Extract: state.id = 42

2. GET /users/42 (id from state)
   → Response: {"id": 42, "name": "Alice"}
   → Check: status 200, schema valid

3. DELETE /users/42
   → Response: 204 No Content

4. GET /users/42 (use-after-free check)
   → Expected: 404 Not Found
```

### Haskell API

```haskell
import Haskemathesis.Stateful

main = do
    spec <- loadOpenApiFile "api.yaml"
    let ops = resolveOperations spec
        config = defaultTestConfig { tcStatefulTesting = True }
    hspec $ do
        describe "Stateful API Tests" $
            it "CRUD sequences work correctly" $
                propertyStateful spec config httpExecutor ops
```

### CLI

```bash
haskemathesis-cli test \
    --spec api.yaml \
    --url http://localhost:8080 \
    --stateful \
    --max-sequence-length 5
```

______________________________________________________________________

## Testing the Implementation

### Unit Tests

```
test/Haskemathesis/Test/Properties/Stateful/
  Links.hs      -- Link parsing and inference
  Extract.hs    -- Value extraction
  Sequence.hs   -- Sequence generation
  Generator.hs  -- State-aware generation
```

### Integration Tests

- Test against a mock CRUD API
- Verify use-after-free detection
- Verify link following

______________________________________________________________________

## Implementation Order

| Order | Phase | Effort | Description |
|-------|-------|--------|-------------|
| 1 | Foundation Types | Low | `Stateful/Types.hs` - Core types |
| 2 | Value Extraction | Medium | `Stateful/Extract.hs` - JSON path, field matching |
| 3 | Links Parsing | Medium | `Stateful/Links.hs` - Parse OpenAPI Links |
| 4 | Heuristic Detection | Medium | Infer links from paths/schemas |
| 5 | State-Aware Gen | Medium | `Stateful/Generator.hs` |
| 6 | Sequencing | High | `Stateful/Sequence.hs` - Generate valid sequences |
| 7 | Stateful Checks | Medium | use-after-free, availability checks |
| 8 | Property Integration | Medium | Add to Property.hs |
| 9 | Config/CLI | Low | Add flags and options |
| 10 | Tests | Medium | Property tests for all new code |

**Estimated total effort:** 3-5 days of focused work

______________________________________________________________________

## Open Questions

1. **Shrinking**: How should Hedgehog shrink stateful sequences? Probably shrink by removing steps from the middle while preserving dependencies.

1. **Parallel execution**: Should stateful tests run in parallel? Probably not - they depend on shared server state.

1. **Cleanup strategy**: Always cleanup? Only on success? Configurable?

1. **Cycle detection**: What if links form a cycle? Need to limit sequence length.

1. **Multiple creation paths**: If both POST and PUT can create resources, how to handle?

______________________________________________________________________

## Comparison with Schemathesis

| Feature | Schemathesis | Haskemathesis (Planned) |
|---------|--------------|-------------------------|
| OpenAPI Links | ✅ | ✅ |
| Path-based inference | ✅ | ✅ |
| Response schema matching | ✅ | ✅ |
| Location header extraction | ✅ | ✅ |
| use-after-free check | ✅ | ✅ |
| ensure-resource-availability | ✅ | ✅ |
| Automatic shrinking | ✅ (Hypothesis) | ✅ (Hedgehog) |
| CLI integration | ✅ | ✅ |

______________________________________________________________________

## Dependencies

No new external dependencies required. Uses:

- `aeson` for JSON manipulation (already a dependency)
- `text` for text processing (already a dependency)
- `containers` for Map (already a dependency)
- `hedgehog` for property testing (already a dependency)
