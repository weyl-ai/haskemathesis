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

renderFailureDetail :: Maybe BaseUrl -> Maybe Text -> FailureDetail -> Text
renderFailureDetail mBase mSeed detail =
    T.intercalate
        "\n"
        ( [ "Check: " <> fdCheck detail
          , "Operation: " <> fdOperation detail
          , "Message: " <> fdMessage detail
          ]
            <> renderSchemaErrors detail
            <> renderSeed mSeed
            <> [ "Request: " <> renderRequest (fdRequest detail)
               , "Response: " <> renderResponse (fdResponse detail)
               , "Curl: " <> toCurl mBase (fdRequest detail)
               ]
        )

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

renderSchemaErrors :: FailureDetail -> [Text]
renderSchemaErrors detail =
    case fdSchemaErrors detail of
        [] -> []
        errs -> ["Schema errors: " <> T.intercalate "; " errs]

renderSeed :: Maybe Text -> [Text]
renderSeed mSeed =
    case mSeed of
        Nothing -> []
        Just seedText -> ["Seed: " <> seedText]
