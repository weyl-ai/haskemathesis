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
)
where

import Data.Map.Strict (Map)
import Data.OpenApi (SecurityRequirement)
import Data.Text (Text)
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
    }
    deriving (Eq, Show)
