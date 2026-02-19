{-# LANGUAGE StrictData #-}

{- | Core types for stateful API testing.

Stateful testing chains API operations together, using responses from earlier
operations to inform requests of later operations. This module defines the
fundamental types used to track state, represent operation links, and
manage resource lifecycles.

=== Overview

Stateful testing enables scenarios like:

@
POST /users          -- Create user, returns {id: 123}
GET /users/123       -- Fetch using id from POST response
PUT /users/123       -- Update using same id
DELETE /users/123    -- Remove the resource
GET /users/123       -- Should return 404 (use-after-free check)
@

=== Key Types

* 'TestState' - Accumulated state during a stateful test run
* 'ResourceRef' - Reference to a created resource for cleanup/verification
* 'OperationLink' - Connection between operations (source -> target)
* 'ValueSource' - Where to get a parameter value (response body, header, etc.)

=== Example

@
import Haskemathesis.Stateful.Types

-- After POST /users returns {"id": 42, "name": "Alice"}
let state = emptyState
      { tsExtractedValues = Map.singleton "id" (Number 42)
      , tsCreatedResources = [ResourceRef "createUser" "/users/{id}" (Map.singleton "id" (Number 42))]
      }
@
-}
module Haskemathesis.Stateful.Types (
    -- * Test State
    TestState (..),
    emptyState,

    -- * Resource References
    ResourceRef (..),

    -- * Operation Links
    OperationLink (..),
    ParameterBinding (..),
    ValueSource (..),
    JsonPath,

    -- * Sequence Types
    OperationSequence (..),
    SequenceStep (..),
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse)

{- | JSON path expression for extracting values from response bodies.

Supports OpenAPI runtime expression syntax:

* @$response.body#/id@ - JSON pointer into response body
* @$response.body#/data/0/id@ - Nested path with array index
* @$.id@ - JSONPath-style (simplified)

Currently we support a subset focused on simple field extraction.
-}
type JsonPath = Text

{- | State accumulated during a stateful test run.

This record tracks everything needed to generate subsequent requests
based on prior responses, and to perform stateful checks like
use-after-free detection.

=== Fields

* 'tsExtractedValues' - Named values extracted from responses, keyed by
  parameter name (e.g., "id", "userId"). Used to fill path/query parameters
  in subsequent requests.

* 'tsCreatedResources' - Resources created during the test that may need
  cleanup or verification. Used for use-after-free checks.

* 'tsHistory' - Complete history of operations executed, for debugging
  and advanced checks.
-}
data TestState = TestState
    { tsExtractedValues :: Map Text Value
    -- ^ Named values extracted from responses (e.g., "id" -> Number 123)
    , tsCreatedResources :: [ResourceRef]
    -- ^ Resources created that should be cleaned up / checked
    , tsHistory :: [(Text, ApiRequest, ApiResponse)]
    -- ^ History: (operationId or label, request, response)
    }
    deriving (Eq, Show)

{- | Empty initial state for starting a stateful test.

@
runStatefulTest = do
    finalState <- foldM executeStep emptyState steps
    checkUseAfterFree finalState
@
-}
emptyState :: TestState
emptyState =
    TestState
        { tsExtractedValues = Map.empty
        , tsCreatedResources = []
        , tsHistory = []
        }

{- | Reference to a resource created during testing.

When a POST or PUT operation creates a resource, we track it here so we can:

1. Clean it up after the test (via DELETE)
2. Verify it's accessible (ensure-resource-availability check)
3. Verify it returns 404 after deletion (use-after-free check)

=== Example

@
-- After POST /users returns {"id": 42}
ResourceRef
    { rrOperationId = "createUser"
    , rrResourcePath = "/users/{id}"
    , rrIdentifiers = Map.singleton "id" (Number 42)
    }
@
-}
data ResourceRef = ResourceRef
    { rrOperationId :: Text
    -- ^ Operation ID that created this resource
    , rrResourcePath :: Text
    -- ^ Path template for accessing the resource (e.g., "/users/{id}")
    , rrIdentifiers :: Map Text Value
    -- ^ Extracted identifiers needed to access the resource
    }
    deriving (Eq, Show)

{- | A link between two operations.

Links define how to call one operation using data from another operation's
response. They can come from:

1. Explicit OpenAPI Links in the spec
2. Heuristic inference from path patterns
3. Response schema to parameter matching

=== Example

@
-- Link from createUser to getUser
OperationLink
    { olSourceOperation = "createUser"
    , olTargetOperation = "getUser"
    , olParameterBindings = [ParameterBinding "id" (FromResponseBody "$response.body#/id")]
    , olDescription = Just "Get the created user"
    , olLinkName = Just "GetUser"
    }
@
-}
data OperationLink = OperationLink
    { olSourceOperation :: Text
    -- ^ Source operation ID or label (the operation whose response provides data)
    , olTargetOperation :: Text
    -- ^ Target operation ID (the operation that consumes the data)
    , olParameterBindings :: [ParameterBinding]
    -- ^ How to fill each parameter in the target operation
    , olDescription :: Maybe Text
    -- ^ Optional description of the link
    , olLinkName :: Maybe Text
    -- ^ Optional name of the link (from OpenAPI spec)
    }
    deriving (Eq, Show)

{- | A binding from a target parameter to its value source.

Represents how to fill a single parameter in a target operation
using data from a source operation's response.

=== Example

@
ParameterBinding
    { pbTargetParam = "userId"
    , pbSource = FromResponseBody "$response.body#/id"
    }
@
-}
data ParameterBinding = ParameterBinding
    { pbTargetParam :: Text
    -- ^ Name of the parameter in the target operation
    , pbSource :: ValueSource
    -- ^ Where to get the value for this parameter
    }
    deriving (Eq, Show)

{- | Source for a parameter value in a stateful request.

When generating a request in a stateful sequence, parameters can come from
various sources rather than being randomly generated.

=== Variants

* 'FromResponseBody' - Extract from the previous response's JSON body
* 'FromResponseHeader' - Extract from a response header (e.g., Location)
* 'FromState' - Use a named value from the accumulated test state
* 'Literal' - Use a fixed value

=== Example

@
-- Extract id from response body
FromResponseBody "$response.body#/id"

-- Extract URL from Location header
FromResponseHeader "Location"

-- Use previously extracted value
FromState "userId"

-- Fixed value
Literal (String "admin")
@
-}
data ValueSource
    = -- | Extract from response body using JSON path
      FromResponseBody JsonPath
    | -- | Extract from response header by name
      FromResponseHeader Text
    | -- | Use a named value from TestState.tsExtractedValues
      FromState Text
    | -- | Use a literal value
      Literal Value
    deriving (Eq, Show)

{- | A sequence of operations to execute in a stateful test.

Represents a complete test scenario, including the main steps and
any cleanup operations (typically DELETEs) to run afterward.

=== Example

@
OperationSequence
    { osSteps =
        [ SequenceStep createUserOp (Map.empty)  -- No bindings, generate randomly
        , SequenceStep getUserOp (Map.singleton "id" (FromState "id"))
        , SequenceStep deleteUserOp (Map.singleton "id" (FromState "id"))
        ]
    , osCleanup = []  -- DELETE already in steps
    }
@
-}
data OperationSequence = OperationSequence
    { osSteps :: [SequenceStep]
    -- ^ Steps to execute in order
    , osCleanup :: [SequenceStep]
    -- ^ Cleanup steps to run after main steps (even on failure)
    }
    deriving (Eq, Show)

{- | A single step in an operation sequence.

Each step pairs an operation with instructions for how to fill its
parameters. Parameters not listed in 'ssParamBindings' will be
generated randomly from their schemas.

=== Fields

* 'ssOperationId' - Which operation to execute
* 'ssParamBindings' - Map from parameter name to value source

=== Example

@
-- Step that uses id from state for the path parameter
SequenceStep
    { ssOperationId = "getUser"
    , ssParamBindings = Map.singleton "id" (FromState "id")
    }
@
-}
data SequenceStep = SequenceStep
    { ssOperationId :: Text
    -- ^ Operation ID to execute
    , ssParamBindings :: Map Text ValueSource
    -- ^ How to fill each parameter (missing = generate from schema)
    }
    deriving (Eq, Show)
