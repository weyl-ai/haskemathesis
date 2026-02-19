{-# LANGUAGE OverloadedStrings #-}

{- | Command-line interface for Haskemathesis.

This module provides a standalone CLI for running property-based API tests
against any HTTP API using an OpenAPI specification. It allows non-Haskell
projects to benefit from Haskemathesis testing without writing Haskell code.

== Usage

Basic usage:

@
haskemathesis test --spec openapi.yaml --url http://localhost:8080
@

With options:

@
haskemathesis test \\
  --spec openapi.yaml \\
  --url http://localhost:8080 \\
  --count 200 \\
  --tag public \\
  --negative
@

== Commands

* @test@ - Run property-based tests against a live API
* @validate@ - Validate an OpenAPI specification without running tests
* @curl@ - Generate curl commands for all operations (dry run)

== Exit Codes

* @0@ - All tests passed or validation succeeded
* @1@ - Tests failed or validation errors found
* @2@ - CLI parsing or configuration error
-}
module Haskemathesis.CLI (
    -- * CLI Entry Point
    runCLI,

    -- * Commands
    Command (..),
    TestOptions (..),
    ValidateOptions (..),
    CurlOptions (..),
    OutputFormat (..),

    -- * Parsing
    parseCommand,
    cliParserInfo,
) where

import Data.Text (Text)
import Options.Applicative

import Haskemathesis.CLI.Options
import Haskemathesis.CLI.Runner (runCurlCommand, runTestCommand, runValidateCommand)

-- | Parse a command from command-line arguments.
parseCommand :: IO Command
parseCommand = execParser cliParserInfo

-- | Main entry point for the CLI.
runCLI :: IO ()
runCLI = do
    cmd <- parseCommand
    case cmd of
        Test opts -> runTestCommand opts
        Validate opts -> runValidateCommand opts
        Curl opts -> runCurlCommand opts

-- | CLI parser info with help text.
cliParserInfo :: ParserInfo Command
cliParserInfo =
    info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Property-based API testing driven by OpenAPI 3.x schemas"
            <> header "haskemathesis - API testing powered by Hedgehog"
        )

-- | Main command parser with subcommands.
commandParser :: Parser Command
commandParser =
    subparser
        ( command "test" (info testCommand (progDesc "Run property-based tests against an API"))
            <> command "validate" (info validateCommand (progDesc "Validate an OpenAPI specification"))
            <> command "curl" (info curlCommand (progDesc "Generate curl commands for API operations"))
        )

-- | Parser for the 'test' command.
testCommand :: Parser Command
testCommand =
    fmap Test $
        TestOptions
            <$> specPathOption
            <*> baseUrlOption
            <*> countOption
            <*> includeOption
            <*> excludeOption
            <*> tagsOption
            <*> negativeSwitch
            <*> authHeaderOption
            <*> outputFormatOption
            <*> seedOption
            <*> timeoutOption
            <*> workersOption
            <*> workdirOption
            <*> streamingTimeoutOption
            <*> maxResponseTimeOption

-- | Parser for the 'validate' command.
validateCommand :: Parser Command
validateCommand =
    fmap Validate $
        ValidateOptions
            <$> specPathOption
            <*> verboseSwitch

-- | Parser for the 'curl' command.
curlCommand :: Parser Command
curlCommand =
    fmap Curl $
        CurlOptions
            <$> specPathOption
            <*> baseUrlOption
            <*> countOption
            <*> includeOption
            <*> excludeOption
            <*> tagsOption

-- | Common option parsers.
specPathOption :: Parser FilePath
specPathOption =
    strOption
        ( long "spec"
            <> short 's'
            <> metavar "FILE"
            <> help "Path to the OpenAPI specification file (YAML or JSON)"
        )

baseUrlOption :: Parser Text
baseUrlOption =
    strOption
        ( long "url"
            <> short 'u'
            <> metavar "URL"
            <> help "Base URL of the API to test (e.g., http://localhost:8080)"
        )

countOption :: Parser Int
countOption =
    option
        auto
        ( long "count"
            <> short 'n'
            <> metavar "N"
            <> value 100
            <> showDefault
            <> help "Number of test cases to generate per operation"
        )

includeOption :: Parser [Text]
includeOption =
    many $
        strOption
            ( long "include"
                <> short 'i'
                <> metavar "PATTERN"
                <> help "Include only operations matching this pattern (can be used multiple times)"
            )

excludeOption :: Parser [Text]
excludeOption =
    many $
        strOption
            ( long "exclude"
                <> short 'e'
                <> metavar "PATTERN"
                <> help "Exclude operations matching this pattern (can be used multiple times)"
            )

tagsOption :: Parser [Text]
tagsOption =
    many $
        strOption
            ( long "tag"
                <> short 't'
                <> metavar "TAG"
                <> help "Filter operations by tag (can be used multiple times)"
            )

negativeSwitch :: Parser Bool
negativeSwitch =
    switch
        ( long "negative"
            <> help "Enable negative testing (generate invalid requests)"
        )

authHeaderOption :: Parser (Maybe Text)
authHeaderOption =
    optional $
        strOption
            ( long "auth-header"
                <> metavar "VALUE"
                <> help "Authorization header value (e.g., 'Bearer token123')"
            )

outputFormatOption :: Parser OutputFormat
outputFormatOption =
    option
        (eitherReader parseOutputFormat)
        ( long "output"
            <> short 'o'
            <> metavar "FORMAT"
            <> value OutputText
            <> showDefaultWith (const "text")
            <> help "Output format: text, json, or junit"
        )
  where
    parseOutputFormat :: String -> Either String OutputFormat
    parseOutputFormat "text" = Right OutputText
    parseOutputFormat "json" = Right OutputJson
    parseOutputFormat "junit" = Right OutputJUnit
    parseOutputFormat s = Left $ "Unknown format: " ++ s ++ ". Use 'text', 'json', or 'junit'."

seedOption :: Parser (Maybe Int)
seedOption =
    optional $
        option
            auto
            ( long "seed"
                <> metavar "INT"
                <> help "Random seed for reproducible tests"
            )

timeoutOption :: Parser (Maybe Int)
timeoutOption =
    optional $
        option
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> help "Request timeout in seconds"
            )

{- | Parser for streaming endpoint timeout.

This controls the default timeout for streaming endpoints (SSE, NDJSON)
that don't have an explicit @x-timeout@ in the OpenAPI spec.
-}
streamingTimeoutOption :: Parser (Maybe Int)
streamingTimeoutOption =
    optional $
        option
            auto
            ( long "streaming-timeout"
                <> metavar "MS"
                <> help "Default timeout in milliseconds for streaming endpoints (default: 1000)"
            )

{- | Parser for max response time check.

If set, tests will fail if any API response takes longer than this threshold.
Useful for performance testing and SLA compliance verification.
-}
maxResponseTimeOption :: Parser (Maybe Int)
maxResponseTimeOption =
    optional $
        option
            auto
            ( long "max-response-time"
                <> metavar "MS"
                <> help "Fail if response time exceeds this limit in milliseconds"
            )

workersOption :: Parser Int
workersOption =
    option
        auto
        ( long "workers"
            <> short 'w'
            <> metavar "N"
            <> value 1
            <> showDefault
            <> help "Number of parallel workers"
        )

verboseSwitch :: Parser Bool
verboseSwitch =
    switch
        ( long "verbose"
            <> short 'v'
            <> help "Show detailed information"
        )

workdirOption :: Parser WorkdirOption
workdirOption =
    option
        (eitherReader parseWorkdir)
        ( long "workdir"
            <> metavar "PATH|temp|current"
            <> value WorkdirTemp
            <> showDefaultWith (const "temp")
            <> help "Working directory: 'temp' (default, cleaned up after), 'current', or a path"
        )
  where
    parseWorkdir :: String -> Either String WorkdirOption
    parseWorkdir "temp" = Right WorkdirTemp
    parseWorkdir "current" = Right WorkdirCurrent
    parseWorkdir path = Right (WorkdirPath path)
