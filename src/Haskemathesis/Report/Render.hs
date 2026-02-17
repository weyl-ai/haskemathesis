{-# LANGUAGE OverloadedStrings #-}

-- | Pretty rendering for failure details.
module Haskemathesis.Report.Render (
    renderFailureDetail,
    renderFailureDetailAnsi,
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
            <> renderSchemaDiff detail
            <> renderMutation detail
            <> renderSeed mSeed
            <> [ "Request: " <> renderRequest (fdRequest detail)
               , "Response: " <> renderResponse (fdResponse detail)
               , "Curl: " <> toCurl mBase (fdRequest detail)
               ]
        )

renderFailureDetailAnsi :: Maybe BaseUrl -> Maybe Text -> FailureDetail -> Text
renderFailureDetailAnsi mBase mSeed detail =
    T.intercalate
        "\n"
        ( [ ansi Cyan ("Check: " <> fdCheck detail)
          , ansi Cyan ("Operation: " <> fdOperation detail)
          , ansi Red ("Message: " <> fdMessage detail)
          ]
            <> map (ansi Red) (renderSchemaErrors detail)
            <> map (ansi Red) (renderSchemaDiff detail)
            <> map (ansi Red) (renderMutation detail)
            <> map (ansi Yellow) (renderSeed mSeed)
            <> [ ansi Green ("Request: " <> renderRequest (fdRequest detail))
               , ansi Green ("Response: " <> renderResponse (fdResponse detail))
               , ansi Green ("Curl: " <> toCurl mBase (fdRequest detail))
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

renderSchemaDiff :: FailureDetail -> [Text]
renderSchemaDiff detail =
    case fdSchemaDiff detail of
        Nothing -> []
        Just diffText ->
            "Schema diff:" : map ("  " <>) (T.lines diffText)

renderMutation :: FailureDetail -> [Text]
renderMutation detail =
    case fdMutation detail of
        Nothing -> []
        Just mutation -> ["Mutation: " <> mutation]

renderSeed :: Maybe Text -> [Text]
renderSeed mSeed =
    case mSeed of
        Nothing -> []
        Just seedText -> ["Seed: " <> seedText]

data AnsiColor
    = Red
    | Green
    | Yellow
    | Cyan

ansi :: AnsiColor -> Text -> Text
ansi color text =
    ansiCode color <> text <> ansiReset

ansiCode :: AnsiColor -> Text
ansiCode color =
    case color of
        Red -> "\ESC[31m"
        Green -> "\ESC[32m"
        Yellow -> "\ESC[33m"
        Cyan -> "\ESC[36m"

ansiReset :: Text
ansiReset = "\ESC[0m"
