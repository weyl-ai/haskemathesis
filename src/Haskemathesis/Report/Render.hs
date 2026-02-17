{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Report.Render (
    renderFailureDetail,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Haskemathesis.Check.Types (FailureDetail (..))
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..), BaseUrl)
import Haskemathesis.Report.Curl (toCurl)

renderFailureDetail :: Maybe BaseUrl -> FailureDetail -> Text
renderFailureDetail mBase detail =
    T.intercalate
        "\n"
        [ "Check: " <> fdCheck detail
        , "Operation: " <> fdOperation detail
        , "Message: " <> fdMessage detail
        , "Request: " <> renderRequest (fdRequest detail)
        , "Response: " <> renderResponse (fdResponse detail)
        , "Curl: " <> toCurl mBase (fdRequest detail)
        ]

renderRequest :: ApiRequest -> Text
renderRequest req =
    T.intercalate
        " "
        [ decodeUtf8 (reqMethod req)
        , reqPath req <> renderQuery (reqQueryParams req)
        ]

renderResponse :: ApiResponse -> Text
renderResponse res =
    "status=" <> T.pack (show (resStatusCode res))

renderQuery :: [(Text, Text)] -> Text
renderQuery [] = ""
renderQuery params =
    "?" <> T.intercalate "&" (map renderPair params)
  where
    renderPair (name, value) = name <> "=" <> value
