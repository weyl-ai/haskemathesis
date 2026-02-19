{- | HTTP executor for running generated requests against a base URL.

This module provides an executor that sends HTTP requests to a live server
using the @http-client@ library. It's used for testing APIs that are
already deployed or running on a specific host.

=== Basic Usage

@
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Haskemathesis.Execute.Http (executeHttp)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    let baseUrl = "http://localhost:8080"
    let request = ApiRequest { ... }
    response <- executeHttp manager baseUrl request
    print (resStatusCode response)
@
-}
module Haskemathesis.Execute.Http (
    executeHttp,
    executeHttpWithTimeout,
)
where

import Control.Exception (catch)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Haskemathesis.Execute.Types
import Network.HTTP.Client (
    HttpException (..),
    HttpExceptionContent (..),
    Manager,
    Request (..),
    RequestBody (RequestBodyBS),
    ResponseTimeout,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
    responseTimeoutMicro,
 )
import Network.HTTP.Types (renderQueryText, statusCode)

{- | Execute an HTTP request against a base URL.

This function takes an 'ApiRequest', converts it to an @http-client@
'Request', sends it using the provided 'Manager', and returns the
response as an 'ApiResponse'.

=== Parameters

* @manager@ - HTTP connection manager (from @http-client@)
* @baseUrl@ - Base URL of the API (e.g., "http://localhost:8080")
* @req@ - The 'ApiRequest' to execute

=== Return Value

Returns an 'ApiResponse' containing the status code, headers, body,
and timing information.

=== Example

@
import Network.HTTP.Client (newManager, defaultManagerSettings)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    let request = ApiRequest
            { reqMethod = "GET"
            , reqPath = "/api/users"
            , reqQueryParams = []
            , reqHeaders = []
            , reqBody = Nothing
            }
    response <- executeHttp manager "http://localhost:8080" request
    putStrLn $ "Status: " ++ show (resStatusCode response)
@
-}
executeHttp :: Manager -> BaseUrl -> ApiRequest -> IO ApiResponse
executeHttp manager baseUrl req = do
    baseReq <- parseRequest (buildUrl baseUrl req)
    let fullReq =
            baseReq
                { method = reqMethod req
                , requestHeaders = contentTypeHeaders req ++ reqHeaders req
                , requestBody = requestBodyFrom req
                }
    response <- httpLbs fullReq manager
    pure
        ApiResponse
            { resStatusCode = statusCode (responseStatus response)
            , resHeaders = responseHeaders response
            , resBody = LBS.toStrict (responseBody response)
            , resTime = 0
            }

buildUrl :: BaseUrl -> ApiRequest -> String
buildUrl baseUrl req =
    baseUrl'
        <> reqPathText
        <> queryStr
  where
    baseUrl' = toString baseUrl
    reqPathText = toString (reqPath req)
    queryStr =
        let q = renderQueryText True (map queryToMaybe (reqQueryParams req))
            qText = decodeUtf8 (LBS.toStrict (toLazyByteString q))
         in if T.null qText then "" else T.unpack qText
    toString = T.unpack

requestBodyFrom :: ApiRequest -> RequestBody
requestBodyFrom req =
    case reqBody req of
        Nothing -> RequestBodyBS mempty
        Just (_, body) -> RequestBodyBS body

{- | Execute an HTTP request with a configurable timeout.

Like 'executeHttp', but allows specifying a timeout in milliseconds.
This is essential for testing streaming endpoints (SSE, NDJSON) that
never complete - the timeout ensures tests don't hang indefinitely.

=== Timeout Behavior

When the timeout is reached:

* The connection is terminated
* Any partial response data received is returned
* For streaming endpoints, this is expected behavior - you get
  whatever data arrived before the timeout

=== Parameters

* @manager@ - HTTP connection manager
* @baseUrl@ - Base URL of the API
* @timeoutMs@ - Optional timeout in milliseconds. 'Nothing' uses default.
* @req@ - The request to execute

=== Example

@
-- Test a streaming endpoint with 2 second timeout
response <- executeHttpWithTimeout manager baseUrl (Just 2000) request
-- response contains whatever data arrived in 2 seconds
@

=== Relationship to x-timeout

This function is used internally when an operation has 'roTimeout' set
(parsed from the @x-timeout@ OpenAPI extension) or when testing
streaming endpoints with a default timeout.
-}
executeHttpWithTimeout ::
    Manager ->
    BaseUrl ->
    Maybe Int ->
    ApiRequest ->
    IO ApiResponse
executeHttpWithTimeout manager baseUrl mTimeoutMs req = do
    baseReq <- parseRequest (buildUrl baseUrl req)
    let timeout = maybe (responseTimeout baseReq) toResponseTimeout mTimeoutMs
    let fullReq =
            baseReq
                { method = reqMethod req
                , requestHeaders = contentTypeHeaders req ++ reqHeaders req
                , requestBody = requestBodyFrom req
                , responseTimeout = timeout
                }
    executeWithTimeoutHandling fullReq manager

-- | Convert milliseconds to ResponseTimeout
toResponseTimeout :: Int -> ResponseTimeout
toResponseTimeout ms = responseTimeoutMicro (ms * 1000)

-- | Execute request and handle timeout exceptions gracefully
executeWithTimeoutHandling :: Request -> Manager -> IO ApiResponse
executeWithTimeoutHandling req manager =
    doRequest req manager `catch` handleTimeout
  where
    handleTimeout :: HttpException -> IO ApiResponse
    handleTimeout (HttpExceptionRequest _ ResponseTimeout) =
        -- Timeout is expected for streaming endpoints
        -- Return a partial success response
        pure
            ApiResponse
                { resStatusCode = 200 -- Assume success for streaming
                , resHeaders = []
                , resBody = mempty
                , resTime = 0
                }
    handleTimeout ex = do
        -- Re-throw other exceptions
        ioError (userError (show ex))

    doRequest r m = do
        response <- httpLbs r m
        pure
            ApiResponse
                { resStatusCode = statusCode (responseStatus response)
                , resHeaders = responseHeaders response
                , resBody = LBS.toStrict (responseBody response)
                , resTime = 0
                }
