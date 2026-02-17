{-# LANGUAGE OverloadedStrings #-}

-- | Render API requests as reproducible curl commands.
module Haskemathesis.Report.Curl (
    toCurl,
) where

import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (hContentType, renderSimpleQuery)

import Haskemathesis.Execute.Types (ApiRequest (..), BaseUrl)

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
