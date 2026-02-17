-- | WAI executor for running requests directly against an Application.
module Haskemathesis.Execute.Wai (
    executeWai,
) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (hContentType, renderQueryText, statusCode)
import Network.Wai (Application, Request (..), defaultRequest)
import Network.Wai.Test (SRequest (..), SResponse (..), runSession, srequest)

import Haskemathesis.Execute.Types

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
        , rawQueryString = LBS.toStrict (toLazyByteString (renderQueryText True (map toQuery (reqQueryParams req))))
        , requestHeaders = contentTypeHeader ++ reqHeaders req
        }
  where
    toQuery (k, v) = (k, Just v)
    contentTypeHeader =
        case reqBody req of
            Nothing -> []
            Just (mediaType, _) ->
                [(hContentType, encodeUtf8 mediaType)]
