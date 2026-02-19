{-# LANGUAGE OverloadedStrings #-}

{- | Test runner implementation for the Haskemathesis CLI.

This module provides the actual test execution logic for the CLI,
including HTTP testing, spec validation, and curl command generation.
-}
module Haskemathesis.CLI.Runner (
    runTestCommand,
    runValidateCommand,
    runCurlCommand,
) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hedgehog (Property, check)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeDirectoryRecursive, setCurrentDirectory)
import System.Environment (setEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

import Haskemathesis.CLI.Internal (buildOperationFilter, buildTestConfig, filterByAnyTag, matchesPattern)
import Haskemathesis.CLI.Options (CurlOptions (..), TestOptions (..), ValidateOptions (..), WorkdirOption (..))
import Haskemathesis.Execute.Http (executeHttpWithTimeout)
import Haskemathesis.Execute.Types (BaseUrl)
import Haskemathesis.OpenApi.Loader (loadOpenApiFile)
import Haskemathesis.OpenApi.Resolve (resolveOperations)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Property (propertiesForSpecWithConfig)

-- | Data structure for test results.
data PropertyResult = PropertyResult
    { propertyName :: !Text
    , propertyPassed :: !Bool
    , propertyTestCount :: !Int
    }

-- | Run the test command.
runTestCommand :: TestOptions -> IO ()
runTestCommand opts = do
    -- Set seed if provided
    case testSeed opts of
        Just seed -> setEnv "HEDGEHOG_SEED" (show seed)
        Nothing -> pure ()

    -- Load the OpenAPI spec (before changing directory so relative paths work)
    specResult <- loadOpenApiFile (testSpecPath opts)
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
                let props = propertiesForSpecWithConfig spec config executor filteredOps

                -- Run tests
                results <- mapM (runProperty baseUrl) props

                -- Report results
                let resultsSeq :: Seq PropertyResult
                    resultsSeq = Seq.fromList results
                let failures = Seq.filter (not . propertyPassed) resultsSeq
                let totalTests = foldl' (+) 0 (fmap propertyTestCount resultsSeq)

                putStrLn "\n=== Test Results ==="
                putStrLn $ "Total operations tested: " <> show (Seq.length resultsSeq)
                putStrLn $ "Total test cases: " <> show totalTests
                putStrLn $ "Failures: " <> show (Seq.length failures)

                if Seq.null failures
                    then exitSuccess
                    else do
                        putStrLn "\nFailed operations:"
                        mapM_ (putStrLn . ("- " <>) . T.unpack . propertyName) failures
                        exitWith (ExitFailure 1)

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
    passed <- check prop
    if passed
        then return $ PropertyResult name True 1
        else return $ PropertyResult name False 1

-- | Run the validate command.
runValidateCommand :: ValidateOptions -> IO ()
runValidateCommand opts = do
    specResult <- loadOpenApiFile (validateSpecPath opts)
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
    specResult <- loadOpenApiFile (curlSpecPath opts)
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
