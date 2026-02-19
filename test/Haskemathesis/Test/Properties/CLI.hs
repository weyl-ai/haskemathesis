{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.CLI (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Options.Applicative (
    ParserResult (..),
    defaultPrefs,
    execParserPure,
 )
import Test.Hspec (Spec, describe)

import Haskemathesis.CLI (cliParserInfo)
import Haskemathesis.CLI.Options (
    CurlOptions (..),
    OutputFormat (..),
    TestOptions (..),
    ValidateOptions (..),
 )
import qualified Haskemathesis.CLI.Options as CLI
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "CLI" $ do
        describe "test command" $ do
            itProp "parses required options" prop_test_required_options
            itProp "parses optional count" prop_test_count_option
            itProp "parses negative flag" prop_test_negative_flag
            itProp "parses auth header" prop_test_auth_header
            itProp "parses output format" prop_test_output_format
            itProp "parses multiple tags" prop_test_multiple_tags
            itProp "parses multiple include patterns" prop_test_multiple_includes
            itProp "parses multiple exclude patterns" prop_test_multiple_excludes
            itProp "parses seed option" prop_test_seed_option
            itProp "parses workers option" prop_test_workers_option
            itProp "parses stateful flag" prop_test_stateful_flag
            itProp "parses max sequence length" prop_test_max_sequence_length
        describe "validate command" $ do
            itProp "parses required options" prop_validate_required_options
            itProp "parses verbose flag" prop_validate_verbose_flag
        describe "curl command" $ do
            itProp "parses required options" prop_curl_required_options
            itProp "parses optional count" prop_curl_count_option
        describe "roundtrip properties" $ do
            itProp "test options roundtrip" prop_test_options_roundtrip
            itProp "validate options roundtrip" prop_validate_options_roundtrip
            itProp "curl options roundtrip" prop_curl_options_roundtrip

-- | Helper to parse CLI arguments.
parseArgs :: [String] -> ParserResult CLI.Command
parseArgs = execParserPure defaultPrefs cliParserInfo

-- | Test command: parses required options (spec and url).
prop_test_required_options :: Property
prop_test_required_options =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl]
        case parseArgs args of
            Success (CLI.Test opts) -> do
                testSpecPath opts === specPath
                testBaseUrl opts === baseUrl
            _parseError -> failure

-- | Test command: parses count option.
prop_test_count_option :: Property
prop_test_count_option =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        count <- forAll $ Gen.int (Range.linear 1 1000)
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--count", show count]
        case parseArgs args of
            Success (CLI.Test opts) -> testCount opts === count
            _parseError -> failure

-- | Test command: parses negative flag.
prop_test_negative_flag :: Property
prop_test_negative_flag =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        let argsWithFlag = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--negative"]
        let argsWithoutFlag = ["test", "--spec", specPath, "--url", T.unpack baseUrl]
        case parseArgs argsWithFlag of
            Success (CLI.Test opts) -> assert (testNegative opts)
            _parseError -> failure
        case parseArgs argsWithoutFlag of
            Success (CLI.Test opts) -> assert (not (testNegative opts))
            _parseError -> failure

-- | Test command: parses auth header option.
prop_test_auth_header :: Property
prop_test_auth_header =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        authValue <- forAll genAuthHeader
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--auth-header", T.unpack authValue]
        case parseArgs args of
            Success (CLI.Test opts) -> testAuthHeader opts === Just authValue
            _parseError -> failure

-- | Test command: parses output format option.
prop_test_output_format :: Property
prop_test_output_format =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        -- Test text format
        let argsText = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--output", "text"]
        case parseArgs argsText of
            Success (CLI.Test opts) -> testOutputFormat opts === OutputText
            _parseError -> failure
        -- Test json format
        let argsJson = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--output", "json"]
        case parseArgs argsJson of
            Success (CLI.Test opts) -> testOutputFormat opts === OutputJson
            _parseError -> failure

-- | Test command: parses multiple tags.
prop_test_multiple_tags :: Property
prop_test_multiple_tags =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        tags <- forAll $ Gen.list (Range.linear 1 5) genTag
        let tagArgs = concatMap (\t -> ["--tag", T.unpack t]) tags
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl] ++ tagArgs
        case parseArgs args of
            Success (CLI.Test opts) -> testTags opts === tags
            _parseError -> failure

-- | Test command: parses multiple include patterns.
prop_test_multiple_includes :: Property
prop_test_multiple_includes =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        patterns <- forAll $ Gen.list (Range.linear 1 3) genPattern
        let patternArgs = concatMap (\p -> ["--include", T.unpack p]) patterns
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl] ++ patternArgs
        case parseArgs args of
            Success (CLI.Test opts) -> testInclude opts === patterns
            _parseError -> failure

-- | Test command: parses multiple exclude patterns.
prop_test_multiple_excludes :: Property
prop_test_multiple_excludes =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        patterns <- forAll $ Gen.list (Range.linear 1 3) genPattern
        let patternArgs = concatMap (\p -> ["--exclude", T.unpack p]) patterns
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl] ++ patternArgs
        case parseArgs args of
            Success (CLI.Test opts) -> testExclude opts === patterns
            _parseError -> failure

-- | Test command: parses seed option.
prop_test_seed_option :: Property
prop_test_seed_option =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        seed <- forAll $ Gen.int (Range.linear 0 maxBound)
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--seed", show seed]
        case parseArgs args of
            Success (CLI.Test opts) -> testSeed opts === Just seed
            _parseError -> failure

-- | Test command: parses workers option.
prop_test_workers_option :: Property
prop_test_workers_option =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        workers <- forAll $ Gen.int (Range.linear 1 16)
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--workers", show workers]
        case parseArgs args of
            Success (CLI.Test opts) -> testWorkers opts === workers
            _parseError -> failure

-- | Test command: parses stateful flag.
prop_test_stateful_flag :: Property
prop_test_stateful_flag =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        let argsWithFlag = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--stateful"]
        let argsWithoutFlag = ["test", "--spec", specPath, "--url", T.unpack baseUrl]
        case parseArgs argsWithFlag of
            Success (CLI.Test opts) -> assert (testStateful opts)
            _parseError -> failure
        case parseArgs argsWithoutFlag of
            Success (CLI.Test opts) -> assert (not (testStateful opts))
            _parseError -> failure

-- | Test command: parses max sequence length option.
prop_test_max_sequence_length :: Property
prop_test_max_sequence_length =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        maxLen <- forAll $ Gen.int (Range.linear 1 20)
        let args = ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--max-sequence-length", show maxLen]
        case parseArgs args of
            Success (CLI.Test opts) -> testMaxSequenceLength opts === maxLen
            _parseError -> failure

-- | Validate command: parses required options.
prop_validate_required_options :: Property
prop_validate_required_options =
    property $ do
        specPath <- forAll genFilePath
        let args = ["validate", "--spec", specPath]
        case parseArgs args of
            Success (CLI.Validate opts) -> validateSpecPath opts === specPath
            _parseError -> failure

-- | Validate command: parses verbose flag.
prop_validate_verbose_flag :: Property
prop_validate_verbose_flag =
    property $ do
        specPath <- forAll genFilePath
        let argsWithFlag = ["validate", "--spec", specPath, "--verbose"]
        let argsWithoutFlag = ["validate", "--spec", specPath]
        case parseArgs argsWithFlag of
            Success (CLI.Validate opts) -> assert (validateVerbose opts)
            _parseError -> failure
        case parseArgs argsWithoutFlag of
            Success (CLI.Validate opts) -> assert (not (validateVerbose opts))
            _parseError -> failure

-- | Curl command: parses required options.
prop_curl_required_options :: Property
prop_curl_required_options =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        let args = ["curl", "--spec", specPath, "--url", T.unpack baseUrl]
        case parseArgs args of
            Success (CLI.Curl opts) -> do
                curlSpecPath opts === specPath
                curlBaseUrl opts === baseUrl
            _parseError -> failure

-- | Curl command: parses count option.
prop_curl_count_option :: Property
prop_curl_count_option =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        count <- forAll $ Gen.int (Range.linear 1 100)
        let args = ["curl", "--spec", specPath, "--url", T.unpack baseUrl, "--count", show count]
        case parseArgs args of
            Success (CLI.Curl opts) -> curlCount opts === count
            _parseError -> failure

-- | Roundtrip property: test options can be serialized to args and parsed back.
prop_test_options_roundtrip :: Property
prop_test_options_roundtrip =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        count <- forAll $ Gen.int (Range.linear 1 500)
        negative <- forAll Gen.bool
        tags <- forAll $ Gen.list (Range.linear 0 3) genTag

        let negativeArg = ["--negative" | negative]
        let tagArgs = concatMap (\t -> ["--tag", T.unpack t]) tags
        let args =
                ["test", "--spec", specPath, "--url", T.unpack baseUrl, "--count", show count]
                    ++ negativeArg
                    ++ tagArgs

        case parseArgs args of
            Success (CLI.Test opts) -> do
                testSpecPath opts === specPath
                testBaseUrl opts === baseUrl
                testCount opts === count
                testNegative opts === negative
                testTags opts === tags
            _parseError -> failure

-- | Roundtrip property: validate options.
prop_validate_options_roundtrip :: Property
prop_validate_options_roundtrip =
    property $ do
        specPath <- forAll genFilePath
        verbose <- forAll Gen.bool

        let verboseArg = ["--verbose" | verbose]
        let args = ["validate", "--spec", specPath] ++ verboseArg

        case parseArgs args of
            Success (CLI.Validate opts) -> do
                validateSpecPath opts === specPath
                validateVerbose opts === verbose
            _parseError -> failure

-- | Roundtrip property: curl options.
prop_curl_options_roundtrip :: Property
prop_curl_options_roundtrip =
    property $ do
        specPath <- forAll genFilePath
        baseUrl <- forAll genUrl
        count <- forAll $ Gen.int (Range.linear 1 100)
        tags <- forAll $ Gen.list (Range.linear 0 3) genTag

        let tagArgs = concatMap (\t -> ["--tag", T.unpack t]) tags
        let args = ["curl", "--spec", specPath, "--url", T.unpack baseUrl, "--count", show count] ++ tagArgs

        case parseArgs args of
            Success (CLI.Curl opts) -> do
                curlSpecPath opts === specPath
                curlBaseUrl opts === baseUrl
                curlCount opts === count
                curlTags opts === tags
            _parseError -> failure

-- | Generators
genFilePath :: Gen FilePath
genFilePath = do
    name <- Gen.string (Range.linear 1 20) Gen.alphaNum
    ext <- Gen.element [".yaml", ".yml", ".json"]
    pure (name ++ ext)

genUrl :: Gen Text
genUrl = do
    protocol <- Gen.element ["http://", "https://"]
    host <- Gen.string (Range.linear 1 15) Gen.alphaNum
    port <- Gen.maybe $ Gen.int (Range.linear 1000 9999)
    let portStr = maybe "" (\p -> ":" ++ show p) port
    pure $ T.pack (protocol ++ host ++ portStr)

genTag :: Gen Text
genTag = T.pack <$> Gen.string (Range.linear 1 15) Gen.alphaNum

genPattern :: Gen Text
genPattern = do
    -- Generate either an operationId-like string or a method+path pattern
    Gen.choice
        [ T.pack <$> Gen.string (Range.linear 3 20) Gen.alphaNum
        , do
            method <- Gen.element ["GET", "POST", "PUT", "DELETE", "PATCH"]
            path <- T.pack <$> Gen.string (Range.linear 1 30) (Gen.frequency [(9, Gen.alphaNum), (1, pure '/')])
            pure $ T.pack method <> " /" <> path
        ]

genAuthHeader :: Gen Text
genAuthHeader = do
    scheme <- Gen.element ["Bearer", "Basic", "ApiKey"]
    token <- Gen.string (Range.linear 10 50) Gen.alphaNum
    pure $ T.pack (scheme ++ " " ++ token)
