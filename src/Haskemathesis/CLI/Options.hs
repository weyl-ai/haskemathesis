{-# LANGUAGE OverloadedStrings #-}

{- | CLI options and types for Haskemathesis.

This module defines the data types used for command-line argument parsing
and configuration. It is separate from the main CLI module to avoid
cyclic dependencies.
-}
module Haskemathesis.CLI.Options (
    Command (..),
    TestOptions (..),
    ValidateOptions (..),
    CurlOptions (..),
    OutputFormat (..),
    WorkdirOption (..),
    defaultTestOptions,
) where

import Data.Text (Text)

-- | Top-level CLI command.
data Command
    = Test !TestOptions
    | Validate !ValidateOptions
    | Curl !CurlOptions
    deriving (Eq, Show)

-- | Options for the 'test' command.
data TestOptions = TestOptions
    { testSpecPath :: !FilePath
    -- ^ Path to the OpenAPI specification file (YAML or JSON)
    , testBaseUrl :: !Text
    -- ^ Base URL of the API to test (e.g., "http://localhost:8080")
    , testCount :: !Int
    -- ^ Number of test cases to generate per operation (default: 100)
    , testInclude :: ![Text]
    -- ^ Include only operations matching these patterns (operationId or method+path)
    , testExclude :: ![Text]
    -- ^ Exclude operations matching these patterns
    , testTags :: ![Text]
    -- ^ Filter operations by tags
    , testNegative :: !Bool
    -- ^ Enable negative testing (generate invalid requests)
    , testAuthHeader :: !(Maybe Text)
    -- ^ Authorization header value (e.g., "Bearer token123")
    , testOutputFormat :: !OutputFormat
    -- ^ Output format for results
    , testSeed :: !(Maybe Int)
    -- ^ Random seed for reproducible tests
    , testTimeout :: !(Maybe Int)
    -- ^ Request timeout in seconds (unused, kept for compatibility)
    , testWorkers :: !Int
    -- ^ Number of parallel workers (default: 1)
    , testWorkdir :: !WorkdirOption
    -- ^ Working directory for tests (to isolate side effects)
    , testStreamingTimeout :: !(Maybe Int)
    {- ^ Default timeout in milliseconds for streaming endpoints (SSE, NDJSON).

    Streaming endpoints never complete normally, so a timeout is required
    to prevent tests from hanging. This is the default timeout applied to
    operations detected as streaming (by content-type) that don't have an
    explicit @x-timeout@ set in the OpenAPI spec.

    Default: 1000ms (1 second)

    Set to 'Nothing' to disable timeout (may cause hangs with streaming APIs).
    -}
    }
    deriving (Eq, Show)

-- | Working directory option for tests.
data WorkdirOption
    = -- | Run in current directory (default)
      WorkdirCurrent
    | -- | Run in a temporary directory (cleaned up after tests)
      WorkdirTemp
    | -- | Run in a specific directory
      WorkdirPath !FilePath
    deriving (Eq, Show)

-- | Options for the 'validate' command.
data ValidateOptions = ValidateOptions
    { validateSpecPath :: !FilePath
    -- ^ Path to the OpenAPI specification file
    , validateVerbose :: !Bool
    -- ^ Show detailed validation information
    }
    deriving (Eq, Show)

-- | Options for the 'curl' command.
data CurlOptions = CurlOptions
    { curlSpecPath :: !FilePath
    -- ^ Path to the OpenAPI specification file
    , curlBaseUrl :: !Text
    -- ^ Base URL for the generated curl commands
    , curlCount :: !Int
    -- ^ Number of example requests to generate per operation
    , curlInclude :: ![Text]
    -- ^ Include only operations matching these patterns
    , curlExclude :: ![Text]
    -- ^ Exclude operations matching these patterns
    , curlTags :: ![Text]
    -- ^ Filter operations by tags
    }
    deriving (Eq, Show)

-- | Output format for test results.
data OutputFormat
    = OutputText
    | OutputJson
    deriving (Eq, Show)

-- | Default test options.
defaultTestOptions :: TestOptions
defaultTestOptions =
    TestOptions
        { testSpecPath = ""
        , testBaseUrl = ""
        , testCount = 100
        , testInclude = []
        , testExclude = []
        , testTags = []
        , testNegative = False
        , testAuthHeader = Nothing
        , testOutputFormat = OutputText
        , testSeed = Nothing
        , testTimeout = Nothing
        , testWorkers = 1
        , testWorkdir = WorkdirTemp
        , testStreamingTimeout = Just 1000
        }
