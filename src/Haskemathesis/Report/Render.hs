{-# LANGUAGE OverloadedStrings #-}

{- | Pretty rendering for failure details.

This module provides functions for rendering 'FailureDetail' records
as human-readable text. It includes both plain text and ANSI-colored
output for better readability in terminal environments.

=== Basic Usage

@
import Haskemathesis.Report.Render (renderFailureDetail)
import Haskemathesis.Check.Types (FailureDetail(..))

let detail = FailureDetail { ... }
    output = renderFailureDetail (Just "http://localhost:8080") Nothing detail
putStrLn (T.unpack output)
@

=== ANSI-Colored Output

For terminal output with colors, use 'renderFailureDetailAnsi':

@
let output = renderFailureDetailAnsi (Just "http://localhost:8080") Nothing detail
@
-}
module Haskemathesis.Report.Render (
    renderFailureDetail,
    renderFailureDetailAnsi,
)
where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Haskemathesis.Check.Types (FailureDetail (..))
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..), BaseUrl)
import Haskemathesis.Report.Curl (toCurl)
import Network.HTTP.Types (hContentType)

{- | Render a failure detail as plain text.

This function converts a 'FailureDetail' into a human-readable text
representation suitable for logging or display. It includes information
about the check that failed, the request and response, and a curl command
for reproducing the issue.

=== Parameters

* @mBase@ - Optional base URL for the curl command
* @mSeed@ - Optional Hedgehog seed for reproducing the test case
* @detail@ - The 'FailureDetail' to render

=== Return Value

Returns a 'Text' containing the formatted failure report.

=== Example

@
let detail = FailureDetail
        { fdCheck = "status_code_conformance"
        , fdMessage = "response status code is not documented: 418"
        , ...
        }
    output = renderFailureDetail (Just "http://localhost:8080") Nothing detail
putStrLn (T.unpack output)
@
-}
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

{- | Render a failure detail with ANSI colors.

This function is similar to 'renderFailureDetail' but adds ANSI color
codes for better readability in terminal environments. Different parts
of the output are color-coded:

* Cyan - Check name and operation
* Red - Error messages and schema issues
* Yellow - Seed information
* Green - Request, response, and curl command

=== Parameters

* @mBase@ - Optional base URL for the curl command
* @mSeed@ - Optional Hedgehog seed for reproducing the test case
* @detail@ - The 'FailureDetail' to render

=== Return Value

Returns a 'Text' containing the ANSI-colored failure report.

=== Example

@
let detail = FailureDetail { ... }
    output = renderFailureDetailAnsi (Just "http://localhost:8080") Nothing detail
putStrLn (T.unpack output)  -- Colored output in terminal
@
-}
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
    T.intercalate ", " $
        ["status=" <> T.pack (show (resStatusCode res))]
            <> contentTypePart
            <> bodyPart
  where
    contentTypePart =
        case lookup hContentType (resHeaders res) of
            Just ct -> ["content-type=" <> decodeUtf8 ct]
            Nothing -> []
    bodyPart =
        let body = resBody res
         in ["body=" <> truncateBody 100 body | not (BS.null body)]

-- | Truncate body to a maximum length, showing ellipsis if truncated.
truncateBody :: Int -> BS.ByteString -> Text
truncateBody maxLen body =
    case decodeUtf8' body of
        Left _ -> "<binary data>"
        Right txt ->
            let stripped = T.strip txt
             in case T.compareLength stripped maxLen of
                    GT -> T.take maxLen stripped <> "..."
                    _ -> stripped

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
