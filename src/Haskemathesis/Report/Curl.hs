{-# LANGUAGE OverloadedStrings #-}

{- | Render API requests as reproducible curl commands.

This module provides functionality to convert 'ApiRequest' values into
equivalent curl command strings. This is useful for debugging failed
tests by providing developers with a command they can run manually to
reproduce the exact request.

=== Basic Usage

@
import Haskemathesis.Report.Curl (toCurl)
import Haskemathesis.Execute.Types (ApiRequest(..))

let request = ApiRequest { ... }
    curlCommand = toCurl (Just "http://localhost:8080") request
putStrLn (T.unpack curlCommand)
-- Output: curl -X GET 'http://localhost:8080/api/users'
@
-}
module Haskemathesis.Report.Curl (
    toCurl,
)
where

import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Haskemathesis.Execute.Types (ApiRequest (..), BaseUrl)
import Network.HTTP.Types (hContentType, renderSimpleQuery)

{- | Convert an API request to a curl command string.

This function renders an 'ApiRequest' as a curl command that can be
executed to reproduce the request. The command includes the HTTP method,
headers, request body, and full URL.

=== Parameters

* @mBase@ - Optional base URL (if not provided, the path alone is used)
* @req@ - The 'ApiRequest' to convert

=== Return Value

Returns a 'Text' containing the curl command.

=== Example

@
let request = ApiRequest
        { reqMethod = "POST"
        , reqPath = "/api/users"
        , reqQueryParams = [("page", "1")]
        , reqHeaders = [("Authorization", "Bearer token123")]
        , reqBody = Just ("application/json", "{\\"name\\":\\"John\\"}")
        }
    curlCmd = toCurl (Just "http://localhost:8080") request
-- Result: curl -X POST -H 'Authorization: Bearer token123' -d '{"name":"John"}' 'http://localhost:8080/api/users?page=1'
@
-}
toCurl :: Maybe BaseUrl -> ApiRequest -> Text
toCurl mBase req =
    T.intercalate " " (filter (not . T.null) parts)
  where
    parts =
        [ "curl"
        , "-X"
        , quote (decodeUtf8 (reqMethod req))
        , headerFlags
        , bodyFlag
        , quote (fullUrl req)
        ]

    headerFlags =
        T.intercalate " " (map renderHeader (headersWithContentType req))

    bodyFlag =
        case reqBody req of
            Nothing -> ""
            Just (_mediaType, body) -> "-d " <> quote (decodeUtf8 body)

    fullUrl request =
        let base = fromMaybe "" mBase
            path = reqPath request
            query = renderQuery (reqQueryParams request)
         in base <> path <> query

renderHeader :: (CI.CI ByteString, ByteString) -> Text
renderHeader (name, value) =
    "-H " <> quote (decodeUtf8 (CI.original name) <> ": " <> decodeUtf8 value)

headersWithContentType :: ApiRequest -> [(CI.CI ByteString, ByteString)]
headersWithContentType req =
    case reqBody req of
        Nothing -> reqHeaders req
        Just (mediaType, _)
            | hasContentType (reqHeaders req) -> reqHeaders req
            | otherwise -> (hContentType, encodeUtf8 mediaType) : reqHeaders req

hasContentType :: [(CI.CI ByteString, ByteString)] -> Bool
hasContentType = any ((== hContentType) . fst)

renderQuery :: [(Text, Text)] -> Text
renderQuery [] = ""
renderQuery params =
    decodeUtf8 (renderSimpleQuery True (map toQuery params))
  where
    toQuery (name, value) = (encodeUtf8 name, encodeUtf8 value)

quote :: Text -> Text
quote txt = "'" <> escapeQuotes txt <> "'"

escapeQuotes :: Text -> Text
escapeQuotes = T.replace "'" "'\\''"
