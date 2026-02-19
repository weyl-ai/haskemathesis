{- | WAI executor for running requests directly against an Application.

This module provides an executor that runs requests against a WAI
'Application' directly in memory, without starting a real HTTP server.
This is useful for fast, deterministic testing of Servant, Yesod, or
any other WAI-based application.

=== Basic Usage

@
import Network.Wai (Application)
import Haskemathesis.Execute.Wai (executeWai)

myApp :: Application
myApp = ... -- your WAI application

main :: IO ()
main = do
    let request = ApiRequest { ... }
    response <- executeWai myApp request
    print (resStatusCode response)
@
-}
module Haskemathesis.Execute.Wai (
    executeWai,
)
where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Haskemathesis.Execute.Types
import Network.HTTP.Types (renderQueryText, statusCode)
import Network.Wai (Application, Request (..), defaultRequest)
import Network.Wai.Test (SRequest (..), SResponse (..), runSession, srequest)

{- | Execute a request against a WAI application.

This function converts an 'ApiRequest' to a WAI 'Request', runs it
through the provided 'Application', and returns the result as an
'ApiResponse'. This allows testing WAI applications without
starting a real HTTP server.

=== Parameters

* @app@ - The WAI 'Application' to test
* @req@ - The 'ApiRequest' to execute

=== Return Value

Returns an 'ApiResponse' containing the status code, headers, body,
and timing information.

=== Example

@
import Network.Wai (Application)
import Haskemathesis.Execute.Wai (executeWai)

myApp :: Application
myApp = ... -- your WAI application

main :: IO ()
main = do
    let request = ApiRequest
            { reqMethod = "GET"
            , reqPath = "/api/users"
            , reqQueryParams = []
            , reqHeaders = []
            , reqBody = Nothing
            }
    response <- executeWai myApp request
    putStrLn $ "Status: " ++ show (resStatusCode response)
@
-}
executeWai :: Application -> ApiRequest -> IO ApiResponse
executeWai app req = do
    let waiReq = toWaiRequest req
    response <- runSession (srequest (SRequest waiReq (LBS.fromStrict body))) app
    pure
        ApiResponse
            { resStatusCode = statusCode (simpleStatus response)
            , resHeaders = simpleHeaders response
            , resBody = LBS.toStrict (simpleBody response)
            , resTime = 0
            }
  where
    body = maybe BS.empty snd (reqBody req)

toWaiRequest :: ApiRequest -> Request
toWaiRequest req =
    defaultRequest
        { requestMethod = reqMethod req
        , rawPathInfo = encodeUtf8 (reqPath req)
        , pathInfo = filter (not . T.null) (T.split (== '/') (reqPath req))
        , rawQueryString = LBS.toStrict (toLazyByteString (renderQueryText True (map queryToMaybe (reqQueryParams req))))
        , requestHeaders = contentTypeHeaders req ++ reqHeaders req
        }
