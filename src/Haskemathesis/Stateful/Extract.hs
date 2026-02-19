{-# LANGUAGE OverloadedStrings #-}

{- | Value extraction from API responses.

This module provides functions for extracting values from API responses
based on 'ValueSource' specifications. Extracted values can be used to
fill parameters in subsequent requests during stateful testing.

=== Supported Extraction Methods

* __JSON Path__ - Extract from response body using path expressions
* __Header__ - Extract from response headers (e.g., Location)
* __Field Name__ - Simple field lookup in JSON objects

=== JSON Path Syntax

We support a subset of OpenAPI runtime expressions:

* @$response.body#\/id@ - JSON pointer into response body
* @$response.body#\/data\/0\/id@ - Nested path with array index
* @$.id@ - Simplified JSONPath-style syntax

=== Example

@
import Haskemathesis.Stateful.Extract
import Haskemathesis.Stateful.Types

-- Extract "id" field from response body
let source = FromResponseBody "$response.body#/id"
    response = ApiResponse 201 [...] "{\"id\": 42, \"name\": \"Alice\"}" 0.1
in extractValue source response
-- Just (Number 42)

-- Extract from Location header
let source = FromResponseHeader "Location"
    response = ApiResponse 201 [("Location", "/users/42")] "" 0.1
in extractValue source response
-- Just (String "/users/42")
@
-}
module Haskemathesis.Stateful.Extract (
    -- * Core Extraction
    extractValue,
    extractFromState,

    -- * Response Body Extraction
    extractByJsonPath,
    extractByFieldName,
    extractByPointer,

    -- * Header Extraction
    extractFromHeader,
    extractFromLocation,

    -- * Path Parsing
    parseJsonPointer,
    parseOpenApiExpression,

    -- * State Updates
    updateStateFromResponse,
    extractIdFields,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Vector as V
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.Stateful.Types (TestState (..), ValueSource (..))
import Text.Read (readMaybe)

{- | Extract a value based on a 'ValueSource' specification.

This is the main entry point for value extraction. It dispatches to
the appropriate extraction method based on the 'ValueSource' variant.

=== Parameters

* @source@ - Where to extract the value from
* @state@ - Current test state (for 'FromState' lookups)
* @response@ - The API response to extract from

=== Return Value

Returns 'Just' the extracted value, or 'Nothing' if extraction fails
(e.g., missing field, invalid path, parse error).

=== Example

@
let source = FromResponseBody "$response.body#/id"
extractValue source state response
-- Just (Number 42)
@
-}
extractValue :: ValueSource -> TestState -> ApiResponse -> Maybe Value
extractValue source state response =
    case source of
        FromResponseBody path ->
            extractByJsonPath path (resBody response)
        FromResponseHeader headerName ->
            extractFromHeader headerName response
        FromState name ->
            extractFromState name state
        Literal value ->
            Just value

{- | Extract a named value from the test state.

Looks up a value by name in the 'tsExtractedValues' map.

=== Example

@
let state = emptyState { tsExtractedValues = Map.singleton "userId" (Number 42) }
extractFromState "userId" state
-- Just (Number 42)
@
-}
extractFromState :: Text -> TestState -> Maybe Value
extractFromState name state =
    Map.lookup name (tsExtractedValues state)

{- | Extract a value from response body using a JSON path expression.

Supports two syntaxes:

* OpenAPI runtime expression: @$response.body#\/field\/path@
* Simplified JSONPath: @$.field.path@ or just @field@

=== Example

@
extractByJsonPath "$response.body#/id" "{\"id\": 42}"
-- Just (Number 42)

extractByJsonPath "$.data.name" "{\"data\": {\"name\": \"Alice\"}}"
-- Just (String "Alice")

extractByJsonPath "id" "{\"id\": 42}"
-- Just (Number 42)
@
-}
extractByJsonPath :: Text -> ByteString -> Maybe Value
extractByJsonPath path body = do
    value <- decodeBody body
    pointer <- parseOpenApiExpression path
    extractByPointer pointer value

-- | Decode response body as JSON.
decodeBody :: ByteString -> Maybe Value
decodeBody = Aeson.decodeStrict

{- | Extract a value using a JSON pointer (list of path segments).

Traverses nested objects and arrays according to the pointer segments.

=== Example

@
let json = Object (KeyMap.singleton "user" (Object (KeyMap.singleton "id" (Number 42))))
extractByPointer ["user", "id"] json
-- Just (Number 42)

let arr = Array (V.fromList [Number 1, Number 2, Number 3])
extractByPointer ["1"] arr
-- Just (Number 2)
@
-}
extractByPointer :: [Text] -> Value -> Maybe Value
extractByPointer [] value = Just value
extractByPointer (segment : rest) value =
    case value of
        Object obj ->
            case KeyMap.lookup (Key.fromText segment) obj of
                Just v -> extractByPointer rest v
                Nothing -> Nothing
        Array arr ->
            case readMaybe (T.unpack segment) of
                Just idx
                    | idx >= 0 && idx < V.length arr ->
                        extractByPointer rest (arr V.! idx)
                _notAnIndex -> Nothing
        _primitive -> Nothing

{- | Extract a value by simple field name lookup.

Only works on JSON objects. For nested paths, use 'extractByJsonPath'.

=== Example

@
let json = Object (KeyMap.singleton "id" (Number 42))
extractByFieldName "id" json
-- Just (Number 42)
@
-}
extractByFieldName :: Text -> Value -> Maybe Value
extractByFieldName fieldName value =
    case value of
        Object obj -> KeyMap.lookup (Key.fromText fieldName) obj
        _notObject -> Nothing

{- | Extract a value from a response header.

Header names are matched case-insensitively.

=== Example

@
let response = ApiResponse 201 [("X-Request-Id", "abc123")] "" 0.1
extractFromHeader "x-request-id" response
-- Just (String "abc123")
@
-}
extractFromHeader :: Text -> ApiResponse -> Maybe Value
extractFromHeader headerName response =
    let headerNameCI = CI.mk (encodeUtf8 headerName)
        headers = resHeaders response
     in case lookup headerNameCI headers of
            Just headerValue ->
                case decodeUtf8' headerValue of
                    Right txt -> Just (String txt)
                    Left _err -> Nothing
            Nothing -> Nothing

{- | Extract a resource URL from the Location header.

Common pattern for REST APIs: POST returns 201 with Location header
pointing to the created resource.

=== Example

@
let response = ApiResponse 201 [("Location", "/users/42")] "" 0.1
extractFromLocation response
-- Just "/users/42"
@
-}
extractFromLocation :: ApiResponse -> Maybe Text
extractFromLocation response =
    case extractFromHeader "Location" response of
        Just (String txt) -> Just txt
        _other -> Nothing

{- | Parse an OpenAPI runtime expression or JSON path into pointer segments.

Supports:

* @$response.body#\/field\/nested@ - OpenAPI runtime expression
* @$.field.nested@ - JSONPath-style
* @field@ - Simple field name

=== Example

@
parseOpenApiExpression "$response.body#/user/id"
-- Just ["user", "id"]

parseOpenApiExpression "$.data.items"
-- Just ["data", "items"]

parseOpenApiExpression "id"
-- Just ["id"]
@
-}
parseOpenApiExpression :: Text -> Maybe [Text]
parseOpenApiExpression expr
    -- OpenAPI runtime expression: $response.body#/path/to/field
    | "$response.body#" `T.isPrefixOf` expr =
        -- Length of "$response.body#" is 16
        let pointer = T.drop 16 expr
         in parseJsonPointer pointer
    -- JSONPath style: $.field.nested
    | "$." `T.isPrefixOf` expr =
        let path = T.drop 2 expr
         in Just (T.splitOn "." path)
    -- Simple field name
    | not (T.null expr) =
        Just [expr]
    | otherwise =
        Nothing

{- | Parse a JSON Pointer (RFC 6901) into path segments.

JSON Pointers start with "/" and use "/" as separator.
Handles escape sequences: ~0 = ~, ~1 = /

=== Example

@
parseJsonPointer "/user/id"
-- Just ["user", "id"]

parseJsonPointer "/data/0/name"
-- Just ["data", "0", "name"]

parseJsonPointer "/field~1with~1slashes"
-- Just ["field/with/slashes"]
@
-}
parseJsonPointer :: Text -> Maybe [Text]
parseJsonPointer pointer
    | T.null pointer = Just []
    | "/" `T.isPrefixOf` pointer =
        let segments = T.splitOn "/" (T.drop 1 pointer)
         in Just (map unescapePointer segments)
    | otherwise =
        -- Not a valid JSON pointer, treat as simple path
        Just (T.splitOn "/" pointer)

{- | Unescape JSON Pointer escape sequences.

* ~0 -> ~
* ~1 -> /
-}
unescapePointer :: Text -> Text
unescapePointer =
    T.replace "~1" "/" . T.replace "~0" "~"

{- | Update test state with values extracted from a response.

This function extracts common ID fields from the response and adds them
to the state's extracted values map.

=== Auto-extracted Fields

* @id@ - Common primary key field
* @_id@ - MongoDB-style ID
* Any field ending in @Id@ or @_id@

=== Example

@
let response = ApiResponse 201 [] "{\"id\": 42, \"userId\": 7}" 0.1
    state = emptyState
    newState = updateStateFromResponse "createUser" state response
-- newState.tsExtractedValues contains "id" -> 42, "userId" -> 7
@
-}
updateStateFromResponse :: Text -> TestState -> ApiResponse -> TestState
updateStateFromResponse _opId state response =
    case decodeBody (resBody response) of
        Just (Object obj) ->
            let extracted = extractIdFields obj
                newValues = Map.union extracted (tsExtractedValues state)
             in state{tsExtractedValues = newValues}
        _notObject ->
            state

{- | Extract fields that look like identifiers from a JSON object.

Identifies fields by name pattern:
* Exactly "id" or "_id"
* Ends with "Id" or "_id" (e.g., "userId", "user_id")
-}
extractIdFields :: Aeson.Object -> Map Text Value
extractIdFields obj =
    Map.fromList
        [ (Key.toText k, v)
        | (k, v) <- KeyMap.toList obj
        , isIdField (Key.toText k)
        , isSimpleValue v
        ]
  where
    isIdField :: Text -> Bool
    isIdField name =
        name == "id"
            || name == "_id"
            || "Id" `T.isSuffixOf` name
            || "_id" `T.isSuffixOf` name

    isSimpleValue :: Value -> Bool
    isSimpleValue (String _) = True
    isSimpleValue (Number _) = True
    isSimpleValue (Bool _) = True
    isSimpleValue _complex = False
