-- | HTTP executor for running generated requests against a base URL.
module Haskemathesis.Execute.Http (
    executeHttp,
) where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client (
    Manager,
    Request (..),
    RequestBody (RequestBodyBS),
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
    responseStatus,
 )
import Network.HTTP.Types (hContentType, renderQueryText, statusCode)

import Haskemathesis.Execute.Types

executeHttp :: Manager -> BaseUrl -> ApiRequest -> IO ApiResponse
executeHttp manager baseUrl req = do
    baseReq <- parseRequest (buildUrl baseUrl req)
    let fullReq =
            baseReq
                { method = reqMethod req
                , requestHeaders = contentTypeHeader ++ reqHeaders req
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
  where
    contentTypeHeader =
        case reqBody req of
            Nothing -> []
            Just (mediaType, _) ->
                [(hContentType, encodeUtf8 mediaType)]

buildUrl :: BaseUrl -> ApiRequest -> String
buildUrl baseUrl req =
    baseUrl'
        <> reqPathText
        <> queryStr
  where
    baseUrl' = toString baseUrl
    reqPathText = toString (reqPath req)
    queryStr =
        let q = renderQueryText True (map toQuery (reqQueryParams req))
            qText = decodeUtf8 (LBS.toStrict (toLazyByteString q))
         in if T.null qText then "" else T.unpack qText
    toQuery (k, v) = (k, Just v)
    toString = T.unpack

requestBodyFrom :: ApiRequest -> RequestBody
requestBodyFrom req =
    case reqBody req of
        Nothing -> RequestBodyBS mempty
        Just (_, body) -> RequestBodyBS body
