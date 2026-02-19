{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for streaming endpoint detection and timeout handling.

These tests verify that:

1. Streaming content types (text/event-stream, application/x-ndjson) are correctly detected
2. x-timeout extensions are properly parsed from OpenAPI specs
3. effectiveTimeout correctly computes timeouts based on config and operation
4. WAI integration correctly filters streaming operations
-}
module Haskemathesis.Test.Properties.Streaming (spec) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.OpenApi.Loader (OperationExtensions (..), extractOperationExtensions)
import Haskemathesis.OpenApi.Types (
    ResolvedOperation (..),
    ResponseSpec (..),
    isStreamingContentType,
    streamingContentTypes,
 )
import Haskemathesis.Property (effectiveTimeout)
import Haskemathesis.Schema (emptySchema)
import Haskemathesis.Test.Support (emptyOperation, itProp)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Streaming" $ do
    describe "isStreamingContentType" $ do
        itProp "detects text/event-stream" propDetectsEventStream
        itProp "detects application/x-ndjson" propDetectsNdjson
        itProp "is case insensitive" propCaseInsensitive
        itProp "ignores charset parameters" propIgnoresCharset
        itProp "rejects non-streaming types" propRejectsNonStreaming

    describe "extractOperationExtensions" $ do
        itProp "extracts x-timeout from operations" propExtractsTimeout
        itProp "handles missing x-timeout" propHandlesMissingTimeout
        itProp "handles multiple operations with different timeouts" propMultipleOperations

    describe "effectiveTimeout" $ do
        itProp "x-timeout takes precedence over streaming default" propXTimeoutPrecedence
        itProp "streaming operations use config timeout" propStreamingUsesConfigTimeout
        itProp "non-streaming without x-timeout has no timeout" propNonStreamingNoTimeout

    describe "WAI streaming filter" $ do
        itProp "partitions streaming from non-streaming ops" propPartitionsStreamingOps
        itProp "preserves all operations in partition" propPreservesAllOps
        itProp "streaming ops have roIsStreaming=True" propStreamingOpsMarked
        itProp "non-streaming ops have roIsStreaming=False" propNonStreamingOpsMarked
        itProp "empty list partitions to empty lists" propEmptyPartition

-- | text/event-stream is detected as streaming
propDetectsEventStream :: Property
propDetectsEventStream = property $ do
    assert (isStreamingContentType "text/event-stream")

-- | application/x-ndjson is detected as streaming
propDetectsNdjson :: Property
propDetectsNdjson = property $ do
    assert (isStreamingContentType "application/x-ndjson")

-- | Content type detection is case insensitive
propCaseInsensitive :: Property
propCaseInsensitive = property $ do
    contentType <- forAll $ Gen.element streamingContentTypes
    -- Test uppercase and lowercase variations
    let upperCase = T.toUpper contentType
        lowerCase = T.toLower contentType
    assert (isStreamingContentType upperCase)
    assert (isStreamingContentType lowerCase)

-- | Charset and other parameters are ignored
propIgnoresCharset :: Property
propIgnoresCharset = property $ do
    charset <- forAll $ Gen.element ["utf-8", "UTF-8", "iso-8859-1"]
    let withCharset = "text/event-stream; charset=" <> charset
    assert (isStreamingContentType withCharset)

-- | Non-streaming content types are rejected
propRejectsNonStreaming :: Property
propRejectsNonStreaming = property $ do
    contentType <-
        forAll $
            Gen.element
                [ "application/json"
                , "text/html"
                , "text/plain"
                , "application/xml"
                , "application/octet-stream"
                ]
    assert (not (isStreamingContentType contentType))

-- | x-timeout is extracted from operation JSON
propExtractsTimeout :: Property
propExtractsTimeout = property $ do
    timeout <- forAll $ Gen.int (Range.linear 100 10000)
    let openApiJson = makeOpenApiWithTimeout "/events" "get" timeout
        extensions = extractOperationExtensions openApiJson
    case Map.lookup ("/events", "GET") extensions of
        Just ext -> oeTimeout ext === Just timeout
        Nothing -> assert False

-- | Missing x-timeout results in Nothing
propHandlesMissingTimeout :: Property
propHandlesMissingTimeout = property $ do
    let openApiJson = makeOpenApiWithoutTimeout "/users" "get"
        extensions = extractOperationExtensions openApiJson
    case Map.lookup ("/users", "GET") extensions of
        Just ext -> oeTimeout ext === Nothing
        Nothing -> assert False

-- | Multiple operations with different timeouts are handled correctly
propMultipleOperations :: Property
propMultipleOperations = property $ do
    timeout1 <- forAll $ Gen.int (Range.linear 100 5000)
    timeout2 <- forAll $ Gen.int (Range.linear 5001 10000)
    let openApiJson = makeOpenApiMultipleOps timeout1 timeout2
        extensions = extractOperationExtensions openApiJson
    case (Map.lookup ("/events", "GET") extensions, Map.lookup ("/stream", "GET") extensions) of
        (Just ext1, Just ext2) -> do
            oeTimeout ext1 === Just timeout1
            oeTimeout ext2 === Just timeout2
        _otherwise -> assert False

-- | x-timeout takes precedence over streaming default
propXTimeoutPrecedence :: Property
propXTimeoutPrecedence = property $ do
    xTimeout <- forAll $ Gen.int (Range.linear 100 5000)
    configTimeout <- forAll $ Gen.int (Range.linear 5001 10000)
    let config = defaultTestConfig{tcStreamingTimeout = Just configTimeout}
        op = makeStreamingOperation (Just xTimeout)
    effectiveTimeout config op === Just xTimeout

-- | Streaming operations without x-timeout use config timeout
propStreamingUsesConfigTimeout :: Property
propStreamingUsesConfigTimeout = property $ do
    configTimeout <- forAll $ Gen.int (Range.linear 100 10000)
    let config = defaultTestConfig{tcStreamingTimeout = Just configTimeout}
        op = makeStreamingOperation Nothing
    effectiveTimeout config op === Just configTimeout

-- | Non-streaming operations without x-timeout have no timeout
propNonStreamingNoTimeout :: Property
propNonStreamingNoTimeout = property $ do
    configTimeout <- forAll $ Gen.int (Range.linear 100 10000)
    let config = defaultTestConfig{tcStreamingTimeout = Just configTimeout}
        op = emptyOperation -- not streaming, no x-timeout
    effectiveTimeout config op === Nothing

-- Helper: Create OpenAPI JSON with x-timeout on an operation
makeOpenApiWithTimeout :: T.Text -> T.Text -> Int -> Value
makeOpenApiWithTimeout path method timeout =
    Object $
        KM.fromList
            [
                ( "paths"
                , Object $
                    KM.fromList
                        [
                            ( fromText path
                            , Object $
                                KM.fromList
                                    [
                                        ( fromText method
                                        , Object $
                                            KM.fromList
                                                [ ("x-timeout", Number (fromIntegral timeout))
                                                , ("responses", Object KM.empty)
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            ]

-- Helper: Create OpenAPI JSON without x-timeout
makeOpenApiWithoutTimeout :: T.Text -> T.Text -> Value
makeOpenApiWithoutTimeout path method =
    Object $
        KM.fromList
            [
                ( "paths"
                , Object $
                    KM.fromList
                        [
                            ( fromText path
                            , Object $
                                KM.fromList
                                    [
                                        ( fromText method
                                        , Object $
                                            KM.fromList
                                                [("responses", Object KM.empty)]
                                        )
                                    ]
                            )
                        ]
                )
            ]

-- Helper: Create OpenAPI JSON with multiple operations
makeOpenApiMultipleOps :: Int -> Int -> Value
makeOpenApiMultipleOps timeout1 timeout2 =
    Object $
        KM.fromList
            [
                ( "paths"
                , Object $
                    KM.fromList
                        [
                            ( "/events"
                            , Object $
                                KM.fromList
                                    [
                                        ( "get"
                                        , Object $
                                            KM.fromList
                                                [ ("x-timeout", Number (fromIntegral timeout1))
                                                , ("responses", Object KM.empty)
                                                ]
                                        )
                                    ]
                            )
                        ,
                            ( "/stream"
                            , Object $
                                KM.fromList
                                    [
                                        ( "get"
                                        , Object $
                                            KM.fromList
                                                [ ("x-timeout", Number (fromIntegral timeout2))
                                                , ("responses", Object KM.empty)
                                                ]
                                        )
                                    ]
                            )
                        ]
                )
            ]

-- Helper: Create a streaming operation with optional x-timeout
makeStreamingOperation :: Maybe Int -> ResolvedOperation
makeStreamingOperation mTimeout =
    emptyOperation
        { roIsStreaming = True
        , roTimeout = mTimeout
        , roResponses =
            Map.singleton
                200
                ResponseSpec
                    { rsContent = Map.singleton "text/event-stream" emptySchema
                    , rsHeaders = Map.empty
                    , rsRequiredHeaders = []
                    }
        }

-- -----------------------------------------------------------------------------
-- WAI streaming filter properties
-- -----------------------------------------------------------------------------

-- | Partition function separates streaming from non-streaming operations
partitionStreamingOps :: [ResolvedOperation] -> ([ResolvedOperation], [ResolvedOperation])
partitionStreamingOps = partition (not . roIsStreaming)

-- | Partitioning correctly separates streaming from non-streaming
propPartitionsStreamingOps :: Property
propPartitionsStreamingOps = property $ do
    numStreaming <- forAll $ Gen.int (Range.linear 0 5)
    numNonStreaming <- forAll $ Gen.int (Range.linear 0 5)
    let streamingOps = replicate numStreaming (makeStreamingOperation Nothing)
        nonStreamingOps = replicate numNonStreaming emptyOperation
        allOps = streamingOps ++ nonStreamingOps
        (nonStr, str) = partitionStreamingOps allOps
    -- Check counts match
    foldl' (\n _ -> n + 1) (0 :: Int) str === numStreaming
    foldl' (\n _ -> n + 1) (0 :: Int) nonStr === numNonStreaming

-- | All operations are preserved after partitioning (none lost)
propPreservesAllOps :: Property
propPreservesAllOps = property $ do
    numStreaming <- forAll $ Gen.int (Range.linear 0 5)
    numNonStreaming <- forAll $ Gen.int (Range.linear 0 5)
    let streamingOps = replicate numStreaming (makeStreamingOperation Nothing)
        nonStreamingOps = replicate numNonStreaming emptyOperation
        allOps = streamingOps ++ nonStreamingOps
        (nonStr, str) = partitionStreamingOps allOps
        totalOriginal = foldl' (\n _ -> n + 1) (0 :: Int) allOps
        totalPartitioned = foldl' (\n _ -> n + 1) (0 :: Int) nonStr + foldl' (\n _ -> n + 1) (0 :: Int) str
    totalPartitioned === totalOriginal

-- | All operations in streaming partition have roIsStreaming=True
propStreamingOpsMarked :: Property
propStreamingOpsMarked = property $ do
    numStreaming <- forAll $ Gen.int (Range.linear 1 5)
    numNonStreaming <- forAll $ Gen.int (Range.linear 0 5)
    let streamingOps = replicate numStreaming (makeStreamingOperation Nothing)
        nonStreamingOps = replicate numNonStreaming emptyOperation
        allOps = streamingOps ++ nonStreamingOps
        (_nonStr, str) = partitionStreamingOps allOps
    -- All streaming ops should have roIsStreaming = True
    assert $ all roIsStreaming str

-- | All operations in non-streaming partition have roIsStreaming=False
propNonStreamingOpsMarked :: Property
propNonStreamingOpsMarked = property $ do
    numStreaming <- forAll $ Gen.int (Range.linear 0 5)
    numNonStreaming <- forAll $ Gen.int (Range.linear 1 5)
    let streamingOps = replicate numStreaming (makeStreamingOperation Nothing)
        nonStreamingOps = replicate numNonStreaming emptyOperation
        allOps = streamingOps ++ nonStreamingOps
        (nonStr, _str) = partitionStreamingOps allOps
    -- All non-streaming ops should have roIsStreaming = False
    assert $ not (any roIsStreaming nonStr)

-- | Empty list partitions to two empty lists
propEmptyPartition :: Property
propEmptyPartition = property $ do
    let (nonStr, str) = partitionStreamingOps []
    nonStr === []
    str === []
