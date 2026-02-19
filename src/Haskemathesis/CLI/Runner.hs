{-# LANGUAGE OverloadedStrings #-}

{- | Test runner implementation for the Haskemathesis CLI.

This module provides the actual test execution logic for the CLI,
including HTTP testing, spec validation, and curl command generation.

=== Features

* __Spec Loading__: Supports both file paths and HTTP/HTTPS URLs
* __Test Execution__: Runs property-based tests against HTTP APIs
* __Output Formats__: Text, JSON, and JUnit XML for CI integration
* __Response Time__: Optional max response time enforcement

=== JUnit XML Output

When using 'OutputJUnit', the runner generates JUnit XML that can be
consumed by CI systems:

@
haskemathesis-cli test --spec api.yaml --url http://localhost:8080 --output junit > results.xml
@

The XML includes test case names, pass/fail status, and timing information.
-}
module Haskemathesis.CLI.Runner (
    runTestCommand,
    runValidateCommand,
    runCurlCommand,
) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (diffUTCTime, getCurrentTime)
import Hedgehog (Property, check)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (setEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

import Haskemathesis.CLI.Internal (buildOperationFilter, buildTestConfig, filterByAnyTag, matchesPattern)
import Haskemathesis.CLI.Options (CurlOptions (..), OutputFormat (..), TestOptions (..), ValidateOptions (..), WorkdirOption (..))
import Haskemathesis.Execute.Http (executeHttpWithTimeout)
import Haskemathesis.Execute.Types (BaseUrl)
import Haskemathesis.OpenApi.Loader (loadOpenApi)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertiesForSpecStateful, propertiesForSpecWithConfig)
import Haskemathesis.Report.JUnit (TestCaseResult (..), renderJUnitXml)

-- | Data structure for test results.
data PropertyResult = PropertyResult
    { propertyName :: !Text
    , propertyPassed :: !Bool
    , propertyTestCount :: !Int
    , propertyTime :: !Double
    -- ^ Time taken in seconds
    }

-- | Run the test command.
runTestCommand :: TestOptions -> IO ()
runTestCommand opts = do
    -- Set seed if provided
    case testSeed opts of
        Just seed -> setEnv "HEDGEHOG_SEED" (show seed)
        Nothing -> pure ()

    -- Load the OpenAPI spec (before changing directory so relative paths work)
    specResult <- loadOpenApi (testSpecPath opts)
    case specResult of
        Left err -> do
            TIO.putStrLn $ "Error loading spec: " <> err
            exitWith (ExitFailure 1)
        Right spec -> do
            -- Run tests in the appropriate working directory
            withWorkdir (testWorkdir opts) $ do
                -- Resolve operations
                let operations = resolveOperations spec

                -- Build operation filter
                let opFilter = buildOperationFilter opts
                let filteredOps = filter opFilter operations

                let filteredOpsSeq :: Seq ResolvedOperation
                    filteredOpsSeq = Seq.fromList filteredOps

                when (Seq.null filteredOpsSeq) $ do
                    putStrLn "No operations match the given filters"
                    exitWith (ExitFailure 1)

                putStrLn $ "Testing " <> show (Seq.length filteredOpsSeq) <> " operation(s)"
                putStrLn $ "Base URL: " <> T.unpack (testBaseUrl opts)
                putStrLn $ "Test count per operation: " <> show (testCount opts)

                -- Create test config
                let config = buildTestConfig opts

                -- Create HTTP manager
                manager <- newManager tlsManagerSettings

                -- Generate properties with timeout-aware executor
                let baseUrl = testBaseUrl opts
                let executor = executeHttpWithTimeout manager baseUrl

                -- Generate normal properties
                let normalProps = propertiesForSpecWithConfig spec config executor filteredOps

                -- Generate stateful properties if enabled
                let statefulProps =
                        if testStateful opts
                            then propertiesForSpecStateful spec config executor filteredOps
                            else []

                let props = normalProps <> statefulProps

                -- Run tests
                results <- mapM (runProperty baseUrl) props

                -- Report results based on output format
                reportResults (testOutputFormat opts) results

{- | Run an action in the specified working directory.
For temp directories, the directory is created before and cleaned up after.
-}
withWorkdir :: WorkdirOption -> IO a -> IO a
withWorkdir WorkdirCurrent action = action
withWorkdir (WorkdirPath path) action = do
    createDirectoryIfMissing True path
    originalDir <- getCurrentDirectory
    bracket
        (setCurrentDirectory path >> return originalDir)
        setCurrentDirectory
        (const action)
withWorkdir WorkdirTemp action = do
    tempBase <- getCanonicalTemporaryDirectory
    originalDir <- getCurrentDirectory
    bracket
        (createTempDirectory tempBase "haskemathesis-test")
        ( \tempDir -> do
            setCurrentDirectory originalDir
            removeDirectoryRecursive tempDir
        )
        ( \tempDir -> do
            putStrLn $ "Running tests in temp directory: " <> tempDir
            setCurrentDirectory tempDir
            action
        )

-- | Run a single property and return results.
runProperty :: BaseUrl -> (Text, Property) -> IO PropertyResult
runProperty _baseUrl (name, prop) = do
    TIO.putStrLn $ "Running: " <> name
    startTime <- getCurrentTime
    passed <- check prop
    endTime <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime endTime startTime) :: Double
    return $ PropertyResult name passed 1 elapsed

-- | Report test results based on output format.
reportResults :: OutputFormat -> [PropertyResult] -> IO ()
reportResults format results = do
    let resultsSeq :: Seq PropertyResult
        resultsSeq = Seq.fromList results
    let failures = Seq.filter (not . propertyPassed) resultsSeq
    let totalTests = foldl' (+) 0 (fmap propertyTestCount resultsSeq)

    case format of
        OutputJUnit -> do
            -- Output JUnit XML to stdout
            let junitResults = map toTestCaseResult results
            TIO.putStrLn $ renderJUnitXml "haskemathesis" junitResults
        OutputJson -> do
            -- Simple JSON output (could be enhanced)
            putStrLn "{"
            putStrLn $ "  \"total_operations\": " <> show (Seq.length resultsSeq) <> ","
            putStrLn $ "  \"total_tests\": " <> show totalTests <> ","
            putStrLn $ "  \"failures\": " <> show (Seq.length failures)
            putStrLn "}"
        OutputText -> do
            putStrLn "\n=== Test Results ==="
            putStrLn $ "Total operations tested: " <> show (Seq.length resultsSeq)
            putStrLn $ "Total test cases: " <> show totalTests
            putStrLn $ "Failures: " <> show (Seq.length failures)

            unless (Seq.null failures) $ do
                putStrLn "\nFailed operations:"
                mapM_ (putStrLn . ("- " <>) . T.unpack . propertyName) failures

    -- Exit with appropriate code
    if Seq.null failures
        then exitSuccess
        else exitWith (ExitFailure 1)

-- | Convert PropertyResult to TestCaseResult for JUnit output.
toTestCaseResult :: PropertyResult -> TestCaseResult
toTestCaseResult pr =
    TestCaseResult
        { tcrName = propertyName pr
        , tcrPassed = propertyPassed pr
        , tcrTime = realToFrac (propertyTime pr)
        , tcrFailureMessage = if propertyPassed pr then Nothing else Just "Property test failed"
        , tcrTestCount = propertyTestCount pr
        }

-- | Run the validate command.
runValidateCommand :: ValidateOptions -> IO ()
runValidateCommand opts = do
    specResult <- loadOpenApi (validateSpecPath opts)
    case specResult of
        Left err -> do
            TIO.putStrLn $ "Validation failed: " <> err
            exitWith (ExitFailure 1)
        Right _ -> do
            TIO.putStrLn "OpenAPI specification is valid"
            when (validateVerbose opts) $
                TIO.putStrLn "Specification loaded successfully"
            exitSuccess

-- | Run the curl command.
runCurlCommand :: CurlOptions -> IO ()
runCurlCommand opts = do
    -- Load the OpenAPI spec
    specResult <- loadOpenApi (curlSpecPath opts)
    case specResult of
        Left err -> do
            TIO.putStrLn $ "Error loading spec: " <> err
            exitWith (ExitFailure 1)
        Right spec -> do
            -- Resolve operations
            let operations = resolveOperations spec

            -- Build operation filter
            let opFilter = buildCurlOperationFilter opts
            let filteredOps = filter opFilter operations
            let filteredOpsSeq :: Seq ResolvedOperation
                filteredOpsSeq = Seq.fromList filteredOps

            when (Seq.null filteredOpsSeq) $ do
                putStrLn "No operations match the given filters"
                exitWith (ExitFailure 1)

            putStrLn $ "Generating curl commands for " <> show (Seq.length filteredOpsSeq) <> " operation(s)"
            putStrLn $ "Base URL: " <> T.unpack (curlBaseUrl opts)
            putStrLn $ "Examples per operation: " <> show (curlCount opts)
            putStrLn ""

            -- For now, just print a message. In a full implementation,
            -- we would generate sample requests and convert them to curl commands
            mapM_
                ( \op -> do
                    putStrLn $ "# " <> T.unpack (fromMaybe (roMethod op <> " " <> roPath op) (roOperationId op))
                    putStrLn $ "# " <> T.unpack (roMethod op) <> " " <> T.unpack (roPath op)
                    putStrLn ""
                )
                filteredOps

            exitSuccess

-- | Build operation filter for curl command.
buildCurlOperationFilter :: CurlOptions -> (ResolvedOperation -> Bool)
buildCurlOperationFilter opts =
    let includeFilter = case curlInclude opts of
            [] -> const True
            patterns -> \op -> any (matchesPattern op) patterns
        excludeFilter = case curlExclude opts of
            [] -> const True
            patterns -> \op -> not (any (matchesPattern op) patterns)
        tagFilter = case curlTags opts of
            [] -> const True
            tags -> filterByAnyTag tags
     in \op -> includeFilter op && excludeFilter op && tagFilter op
