{-# LANGUAGE OverloadedStrings #-}

{- | State-aware request generation for stateful API testing.

This module provides generators that use accumulated test state to fill
parameter values, enabling realistic request sequences where later requests
use data from earlier responses.

=== Key Concepts

* __State-Aware Generation__: Parameters can be filled from test state
  instead of randomly generated
* __Fallback Generation__: If state doesn't have a value, fall back to
  schema-based random generation
* __State Updates__: After each request, extract values from response
  and update state for subsequent requests

=== Example

@
import Haskemathesis.Stateful.Generator
import Haskemathesis.Stateful.Types

-- Generate a GET request using state for the id parameter
let state = emptyState { tsExtractedValues = Map.singleton "id" (Number 42) }
    bindings = Map.singleton "id" (FromState "id")
request <- genStatefulRequest state bindings getUserOp
-- request.reqPath will be "/users/42"
@
-}
module Haskemathesis.Stateful.Generator (
    -- * Request Generation
    genStatefulRequest,
    genStatefulParam,

    -- * State Management
    updateState,
    fillParamFromState,
    resolveBinding,

    -- * Value Rendering
    renderValueForParam,

    -- * Helpers
    applyBindings,
    getRequiredBindings,
) where

import Data.Aeson (Value (..), encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hedgehog (Gen)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.URI (urlEncode)

import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.Gen (genFromSchema)
import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
    ResolvedRequestBody (..),
 )
import Haskemathesis.Stateful.Extract (
    extractIdFields,
    extractValue,
 )
import Haskemathesis.Stateful.Types (
    OperationLink (..),
    ParameterBinding (..),
    ResourceRef (..),
    SequenceStep (..),
    TestState (..),
    ValueSource (..),
 )

{- | Generate a request using test state to fill parameter values.

This is the main entry point for state-aware request generation. It uses
the provided bindings to fill parameters from state, falling back to
schema-based generation for unbound parameters.

=== Parameters

* @state@ - Current test state with extracted values
* @step@ - Sequence step with parameter bindings
* @op@ - The operation to generate a request for

=== Return Value

Returns a 'Gen ApiRequest' that generates requests with state-derived values.

=== Example

@
let state = emptyState { tsExtractedValues = Map.singleton "userId" (Number 42) }
    step = SequenceStep "getUser" (Map.singleton "userId" (FromState "userId"))
    op = ... -- ResolvedOperation for GET /users/{userId}
request <- genStatefulRequest state step op
-- request.reqPath will be "/users/42"
@
-}
genStatefulRequest ::
    TestState ->
    SequenceStep ->
    ResolvedOperation ->
    Gen ApiRequest
genStatefulRequest state step op = do
    let bindings = ssParamBindings step
    params <- traverse (genStatefulParam state bindings) (roParameters op)
    body <- traverse genRequestBody (roRequestBody op)
    let (pathParams, queryParams, headerParams) = partitionParams params
    let path = interpolatePath (roPath op) pathParams
    pure
        ApiRequest
            { reqMethod = encodeUtf8 (roMethod op)
            , reqPath = path
            , reqQueryParams = queryParams
            , reqHeaders = headerParams
            , reqBody = body
            }

{- | Generate a parameter value using state or schema.

If the parameter has a binding in the provided map and the binding
can be resolved from state, use that value. Otherwise, generate
a value from the parameter's schema.

=== Parameters

* @state@ - Current test state
* @bindings@ - Map from parameter name to value source
* @param@ - The parameter to generate a value for

=== Return Value

Returns (location, (name, renderedValue)) for placement in the request.
-}
genStatefulParam ::
    TestState ->
    Map Text ValueSource ->
    ResolvedParam ->
    Gen (ParamLocation, (Text, Text))
genStatefulParam state bindings param = do
    let paramName = rpName param
    value <- case Map.lookup paramName bindings of
        Just source ->
            -- Try to resolve from state/bindings first
            case resolveBinding source state of
                Just v -> pure v
                Nothing -> genFromSchema (rpSchema param)
        Nothing ->
            -- No binding, generate from schema
            genFromSchema (rpSchema param)
    pure (rpLocation param, (paramName, renderValueForParam value))

{- | Resolve a value source to a concrete value.

Attempts to extract the value based on the source type:

* 'FromState' - Look up in state's extracted values
* 'Literal' - Use the literal value directly
* 'FromResponseBody' / 'FromResponseHeader' - These require a response,
  so they return Nothing (should be resolved during execution)

=== Parameters

* @source@ - The value source to resolve
* @state@ - Current test state

=== Return Value

'Just Value' if resolved, 'Nothing' if the value isn't available.
-}
resolveBinding :: ValueSource -> TestState -> Maybe Value
resolveBinding source state =
    case source of
        FromState name ->
            Map.lookup name (tsExtractedValues state)
        Literal value ->
            Just value
        FromResponseBody _ ->
            -- Requires response context, can't resolve at generation time
            Nothing
        FromResponseHeader _ ->
            -- Requires response context, can't resolve at generation time
            Nothing

{- | Try to fill a parameter value from test state.

This is a simpler interface when you just want to check if a value
is available in state without the full binding resolution.

=== Parameters

* @state@ - Current test state
* @paramName@ - Name of the parameter to look up

=== Return Value

'Just Value' if found in state, 'Nothing' otherwise.
-}
fillParamFromState :: TestState -> Text -> Maybe Value
fillParamFromState state paramName =
    Map.lookup paramName (tsExtractedValues state)

{- | Update test state after executing a request.

This function:
1. Adds the request/response to history
2. Extracts ID-like fields from the response body
3. Tracks created resources for cleanup and checks

=== Parameters

* @opId@ - Operation ID or label
* @op@ - The executed operation
* @req@ - The request that was sent
* @res@ - The response that was received
* @state@ - Current test state

=== Return Value

Updated 'TestState' with new extracted values and history.

=== Example

@
let state = emptyState
    response = ApiResponse 201 [] "{\"id\": 42, \"name\": \"Alice\"}" 0.1
    newState = updateState "createUser" createUserOp request response state
-- newState.tsExtractedValues will contain "id" -> Number 42
-- newState.tsHistory will have the request/response recorded
@
-}
updateState ::
    Text ->
    ResolvedOperation ->
    ApiRequest ->
    ApiResponse ->
    TestState ->
    TestState
updateState opId op req res state =
    let
        -- Add to history
        newHistory = (opId, req, res) : tsHistory state

        -- Extract values from response body
        newExtracted = case decodeResponseBody (resBody res) of
            Just (Object obj) ->
                Map.union (extractIdFields obj) (tsExtractedValues state)
            Just (Array _) -> tsExtractedValues state
            Just (String _) -> tsExtractedValues state
            Just (Number _) -> tsExtractedValues state
            Just (Bool _) -> tsExtractedValues state
            Just Null -> tsExtractedValues state
            Nothing -> tsExtractedValues state

        -- Track created resources (for POST operations with 2xx response)
        newResources =
            if isCreateOperation op && isSuccessResponse res
                then createResourceRef opId op newExtracted : tsCreatedResources state
                else tsCreatedResources state
     in
        state
            { tsHistory = newHistory
            , tsExtractedValues = newExtracted
            , tsCreatedResources = newResources
            }

{- | Render a JSON value as text for use in a parameter.

Handles all JSON value types appropriately:
* Strings - rendered directly (no quotes)
* Numbers - numeric representation
* Booleans - "true" or "false"
* Null - "null"
* Arrays/Objects - JSON-encoded
-}
renderValueForParam :: Value -> Text
renderValueForParam value =
    case value of
        String txt -> txt
        Number n -> T.pack (show n)
        Bool True -> "true"
        Bool False -> "false"
        Null -> "null"
        Array _ -> decodeUtf8 (LBS.toStrict (encode value))
        Object _ -> decodeUtf8 (LBS.toStrict (encode value))

{- | Apply bindings from a link to create a sequence step.

Given an operation link and the resolved values, creates a SequenceStep
with the appropriate parameter bindings.

=== Parameters

* @link@ - The operation link with parameter bindings
* @state@ - Current state (for resolving response-based bindings)
* @response@ - Response from the source operation

=== Return Value

A 'SequenceStep' ready for execution.
-}
applyBindings ::
    OperationLink ->
    TestState ->
    ApiResponse ->
    SequenceStep
applyBindings link state response =
    let bindings =
            Map.fromList
                [ (pbTargetParam pb, resolvedSource pb)
                | pb <- olParameterBindings link
                ]
     in SequenceStep
            { ssOperationId = olTargetOperation link
            , ssParamBindings = bindings
            }
  where
    -- Resolve response-based sources to state-based sources
    resolvedSource :: ParameterBinding -> ValueSource
    resolvedSource pb =
        case pbSource pb of
            FromResponseBody path ->
                -- Try to extract and store in state, then reference
                case extractValue (FromResponseBody path) state response of
                    Just val -> Literal val
                    Nothing -> pbSource pb
            FromResponseHeader hdr ->
                case extractValue (FromResponseHeader hdr) state response of
                    Just val -> Literal val
                    Nothing -> pbSource pb
            other -> other

{- | Get the list of required parameter bindings for an operation.

Returns the names of all required path parameters that need bindings.

=== Parameters

* @op@ - The operation to analyze

=== Return Value

List of parameter names that must be bound.
-}
getRequiredBindings :: ResolvedOperation -> [Text]
getRequiredBindings op =
    [ rpName p
    | p <- roParameters op
    , rpLocation p == ParamPath
    , rpRequired p
    ]

-- Internal helpers

decodeResponseBody :: BS.ByteString -> Maybe Value
decodeResponseBody = Aeson.decodeStrict

isCreateOperation :: ResolvedOperation -> Bool
isCreateOperation op = roMethod op == "POST"

isSuccessResponse :: ApiResponse -> Bool
isSuccessResponse res =
    let code = resStatusCode res
     in code >= 200 && code < 300

createResourceRef :: Text -> ResolvedOperation -> Map Text Value -> ResourceRef
createResourceRef opId op extracted =
    ResourceRef
        { rrOperationId = opId
        , rrResourcePath = roPath op
        , rrIdentifiers = extracted
        }

genRequestBody :: ResolvedRequestBody -> Gen (Text, BS.ByteString)
genRequestBody body = do
    value <- genFromSchema (rbSchema body)
    pure (rbContentType body, LBS.toStrict (encode value))

partitionParams ::
    [(ParamLocation, (Text, Text))] ->
    ([(Text, Text)], [(Text, Text)], [(HeaderName, BS.ByteString)])
partitionParams =
    foldr step ([], [], [])
  where
    step (location, (name, value)) (paths, queries, headers) =
        case location of
            ParamPath -> ((name, value) : paths, queries, headers)
            ParamQuery -> (paths, (name, value) : queries, headers)
            ParamHeader -> (paths, queries, toHeader name value : headers)
            ParamCookie -> (paths, queries, headers)

toHeader :: Text -> Text -> (HeaderName, BS.ByteString)
toHeader name value = (CI.mk (encodeUtf8 name), encodeUtf8 value)

interpolatePath :: Text -> [(Text, Text)] -> Text
interpolatePath = foldl' replace
  where
    replace acc (name, value) =
        T.replace ("{" <> name <> "}") (encodePathSegment value) acc

encodePathSegment :: Text -> Text
encodePathSegment = decodeUtf8 . urlEncode False . encodeUtf8
