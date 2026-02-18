{-# LANGUAGE StrictData #-}

{- | Request/response types shared by executors and checks.

This module defines the core data types used for representing
API requests and responses throughout Haskemathesis. These types
are used by executors to perform HTTP calls and by checks to
validate responses.

=== Building Requests

Construct a request for execution:

@
import Network.HTTP.Types (methodPost)

request :: ApiRequest
request = ApiRequest
    { reqMethod = methodPost
    , reqPath = "/api/users"
    , reqQueryParams = [("page", "1")]
    , reqHeaders = [("Content-Type", "application/json")]
    , reqBody = Just ("application/json", "{\\"name\\":\\"test\\"}")
    }
@

=== Creating Responses

When writing custom executors or mocks:

@
response :: ApiResponse
response = ApiResponse
    { resStatusCode = 200
    , resHeaders = [("Content-Type", "application/json")]
    , resBody = "{\\"id\\": 1}"
    , resTime = 0.1
    }
@
-}
module Haskemathesis.Execute.Types (
    ApiRequest (..),
    ApiResponse (..),
    MediaType,
    BaseUrl,
)
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.HTTP.Types (HeaderName, Method)

-- | Media type as a text string (e.g., "application/json").
type MediaType = Text

-- | Base URL for the API as a text string (e.g., "http://localhost:8080").
type BaseUrl = Text

{- | Represents an HTTP request to be sent to the API.

This type captures all the information needed to make an API call,
including the HTTP method, path, query parameters, headers, and body.
-}
data ApiRequest = ApiRequest
    { reqMethod :: Method
    -- ^ HTTP method (e.g., GET, POST, PUT, DELETE).
    , reqPath :: Text
    -- ^ URL path (e.g., "/api/users").
    , reqQueryParams :: [(Text, Text)]
    -- ^ Query parameters as key-value pairs.
    , reqHeaders :: [(HeaderName, ByteString)]
    -- ^ HTTP headers as key-value pairs.
    , reqBody :: Maybe (MediaType, ByteString)
    -- ^ Optional request body with its media type.
    }
    deriving (Eq, Show)

{- | Represents an HTTP response received from the API.

This type captures the response status code, headers, body,
and timing information for analysis by checks.
-}
data ApiResponse = ApiResponse
    { resStatusCode :: Int
    -- ^ HTTP status code (e.g., 200, 404, 500).
    , resHeaders :: [(HeaderName, ByteString)]
    -- ^ Response headers as key-value pairs.
    , resBody :: ByteString
    -- ^ Raw response body as a bytestring.
    , resTime :: NominalDiffTime
    -- ^ Time taken for the request/response cycle.
    }
    deriving (Eq, Show)
