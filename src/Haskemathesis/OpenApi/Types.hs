{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | Resolved OpenAPI types used by generators and checks.

This module provides a simplified, resolved representation of OpenAPI
operations. The types here are "resolved" in the sense that all
references have been flattened and the data structures are normalized
for easy consumption by generators and checks.

=== Type Hierarchy

The main type is 'ResolvedOperation', which represents a single API
endpoint (combination of HTTP method and path). It contains:

* 'ResolvedParam' - Parameters (path, query, header, cookie)
* 'ResolvedRequestBody' - Request body specification
* 'ResponseSpec' - Expected response specifications

=== Usage

These types are typically obtained by calling 'resolveOperations' on
a loaded OpenAPI specification:

@
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)

main :: IO ()
main = do
    Right spec <- loadOpenApiFile "api.yaml"
    let ops = resolveOperations spec :: [ResolvedOperation]
    -- Use operations for testing...
@
-}
module Haskemathesis.OpenApi.Types (
    -- * Parameter Types
    ParamLocation (..),
    ResolvedParam (..),

    -- * Request/Response Types
    ResolvedRequestBody (..),
    ResponseSpec (..),

    -- * Operation Type
    ResolvedOperation (..),

    -- * Streaming Detection
    isStreamingContentType,
    streamingContentTypes,
)
where

import Data.Map.Strict (Map)
import Data.OpenApi (SecurityRequirement)
import Data.Text (Text)
import qualified Data.Text as T
import Haskemathesis.Schema (Schema)

{- | Location of a parameter in an HTTP request.

OpenAPI supports parameters in four locations:

* 'ParamPath' - URL path segments (e.g., @/users/{id}@)
* 'ParamQuery' - URL query string (e.g., @?page=1@)
* 'ParamHeader' - HTTP headers
* 'ParamCookie' - Cookie values
-}
data ParamLocation
    = ParamPath
    | ParamQuery
    | ParamHeader
    | ParamCookie
    deriving (Eq, Show)

{- | A resolved parameter definition.

Represents a single parameter after all OpenAPI references have been
resolved. Contains the parameter name, location, whether it's required,
and its JSON schema.
-}
data ResolvedParam = ResolvedParam
    { rpName :: Text
    -- ^ Parameter name (e.g., "userId", "page", "Authorization").
    , rpLocation :: ParamLocation
    -- ^ Where the parameter appears in the request.
    , rpRequired :: Bool
    -- ^ Whether the parameter is required. Optional parameters may be omitted.
    , rpSchema :: Schema
    -- ^ JSON Schema defining the parameter's type and constraints.
    }
    deriving (Eq, Show)

{- | A resolved request body definition.

Represents the request body specification after resolution.
Currently only the first content type is used.
-}
data ResolvedRequestBody = ResolvedRequestBody
    { rbContentType :: Text
    -- ^ Content type of the request body (e.g., "application/json").
    , rbSchema :: Schema
    -- ^ JSON Schema defining the request body structure.
    }
    deriving (Eq, Show)

{- | Specification for an expected response.

Defines what a successful response should look like, including
the content schema and header definitions.
-}
data ResponseSpec = ResponseSpec
    { rsContent :: Map Text Schema
    -- ^ Map from content type (e.g., "application/json") to schema.
    , rsHeaders :: Map Text Schema
    -- ^ Map from header name to schema for that header's value.
    , rsRequiredHeaders :: [Text]
    -- ^ List of header names that must be present in the response.
    }
    deriving (Eq, Show)

{- | A fully resolved OpenAPI operation.

This is the central type representing a single API endpoint
(combination of HTTP method and path) with all references resolved.
It contains all the information needed to generate requests and
validate responses for that endpoint.

=== Fields

* 'roMethod' - HTTP method (GET, POST, etc.)
* 'roPath' - URL path template (e.g., "/users/{id}")
* 'roOperationId' - Optional operation identifier from OpenAPI
* 'roTags' - Tags for grouping/categorizing operations
* 'roParameters' - List of parameters (path, query, header, cookie)
* 'roRequestBody' - Optional request body specification
* 'roResponses' - Map of status codes to response specifications
* 'roDefaultResponse' - Optional default response specification
* 'roSecurity' - Security requirements for this operation
* 'roIsStreaming' - Whether this operation returns a streaming response
* 'roTimeout' - Optional request timeout in milliseconds (from @x-timeout@)
-}
data ResolvedOperation = ResolvedOperation
    { roMethod :: Text
    -- ^ HTTP method (e.g., "GET", "POST", "PUT", "DELETE").
    , roPath :: Text
    -- ^ URL path template (e.g., "/api/users", "/users/{id}").
    , roOperationId :: Maybe Text
    {- ^ Optional operation identifier from the OpenAPI spec.
    Used for labeling tests when available.
    -}
    , roTags :: [Text]
    -- ^ Tags for categorizing/grouping operations.
    , roParameters :: [ResolvedParam]
    -- ^ Parameters for this operation (path, query, header, cookie).
    , roRequestBody :: Maybe ResolvedRequestBody
    -- ^ Optional request body specification.
    , roResponses :: Map Int ResponseSpec
    {- ^ Map of HTTP status codes to response specifications.
    Only includes explicitly documented status codes.
    -}
    , roDefaultResponse :: Maybe ResponseSpec
    {- ^ Optional default response that applies to status codes
    not explicitly documented in 'roResponses'.
    -}
    , roSecurity :: [SecurityRequirement]
    {- ^ Security requirements for this operation.
    If empty, the operation requires no authentication.
    -}
    , roIsStreaming :: Bool
    {- ^ Whether this operation returns a streaming response.

    An operation is considered streaming if any of its success responses
    (2xx status codes) include one of these content types:

    * @text/event-stream@ - Server-Sent Events (SSE)
    * @application/x-ndjson@ - Newline-delimited JSON

    Streaming operations are tested with a timeout to prevent tests
    from hanging indefinitely. See 'roTimeout' for timeout configuration.
    -}
    , roTimeout :: Maybe Int
    {- ^ Optional request timeout in milliseconds.

    This value is parsed from the @x-timeout@ OpenAPI extension, which is
    a __non-standard vendor extension__ that Haskemathesis recognizes:

    @
    \/events:
      get:
        x-timeout: 2000  # Timeout in milliseconds
        responses:
          "200":
            content:
              text/event-stream: {}
    @

    __Timeout behavior:__

    * If @x-timeout@ is specified, that value is used
    * If the operation is streaming ('roIsStreaming' is 'True') and no
      @x-timeout@ is set, a default streaming timeout is applied
      (configurable via 'tcStreamingTimeout' in 'TestConfig')
    * For non-streaming operations without @x-timeout@, no timeout is applied
      (or the HTTP client's default timeout is used)

    __Why milliseconds?__

    Milliseconds provide fine-grained control needed for streaming endpoints
    where you want to capture "some" data quickly (e.g., 500ms) without
    waiting for the stream to complete (which may never happen).
    -}
    }
    deriving (Eq, Show)

{- | List of content types that indicate streaming responses.

These content types are used to automatically detect streaming operations:

* @text/event-stream@ - Server-Sent Events (SSE), used for real-time updates
* @application/x-ndjson@ - Newline-delimited JSON, used for streaming JSON records

When an operation's success responses include any of these content types,
the operation is marked as streaming ('roIsStreaming' = 'True').
-}
streamingContentTypes :: [Text]
streamingContentTypes =
    [ "text/event-stream"
    , "application/x-ndjson"
    ]

{- | Check if a content type indicates a streaming response.

Performs case-insensitive matching and ignores parameters (charset, etc.):

>>> isStreamingContentType "text/event-stream"
True

>>> isStreamingContentType "TEXT/EVENT-STREAM; charset=utf-8"
True

>>> isStreamingContentType "application/json"
False
-}
isStreamingContentType :: Text -> Bool
isStreamingContentType contentType =
    normalizedType `elem` streamingContentTypes
  where
    -- Strip parameters (e.g., "; charset=utf-8") and lowercase
    normalizedType = T.toLower (T.strip (T.takeWhile (/= ';') contentType))
