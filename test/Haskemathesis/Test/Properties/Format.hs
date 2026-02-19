{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for format generators.

These tests verify that format generators produce valid values:

1. IPv4 addresses have valid format (4 octets 0-255)
2. IPv6 addresses have valid format (8 groups of hex)
3. Base64 (byte) values are valid base64
4. Hostnames/URIs have valid format
-}
module Haskemathesis.Test.Properties.Format (spec) where

import Data.Aeson (Value (..))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, annotateShow, assert, forAll, property)
import Test.Hspec (Spec, describe)
import Text.Read (readMaybe)

import Haskemathesis.Gen.Format (genByte, genIPv4, genIPv6, genURI, genUUID)
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "Format Generators" $ do
        describe "genIPv4" $ do
            itProp "generates valid IPv4 format" propIPv4ValidFormat
            itProp "generates octets in valid range" propIPv4ValidOctets

        describe "genIPv6" $ do
            itProp "generates valid IPv6 format" propIPv6ValidFormat
            itProp "generates valid hex groups" propIPv6ValidHex

        describe "genByte" $ do
            itProp "generates valid base64" propByteValidBase64
            itProp "decodes to bytes" propByteDecodes

        describe "genURI" $ do
            itProp "generates valid URI format" propURIValidFormat
            itProp "starts with https://" propURIStartsWithHttps

        describe "genUUID" $ do
            itProp "generates valid UUID format" propUUIDValidFormat
            itProp "has correct segment lengths" propUUIDSegmentLengths

-- IPv4 properties

propIPv4ValidFormat :: Property
propIPv4ValidFormat = property $ do
    value <- forAll genIPv4
    case value of
        String txt -> do
            let parts = T.splitOn "." txt
            annotateShow parts
            assert $ length parts == (4 :: Int)
        _unexpectedValue -> assert False

propIPv4ValidOctets :: Property
propIPv4ValidOctets = property $ do
    value <- forAll genIPv4
    case value of
        String txt -> do
            let parts = T.splitOn "." txt
                octets = map (readMaybe . T.unpack) parts :: [Maybe Int]
            annotateShow octets
            assert $ all isValidOctet octets
        _unexpectedValue -> assert False
  where
    isValidOctet :: Maybe Int -> Bool
    isValidOctet (Just n) = n >= 0 && n <= 255
    isValidOctet Nothing = False

-- IPv6 properties

propIPv6ValidFormat :: Property
propIPv6ValidFormat = property $ do
    value <- forAll genIPv6
    case value of
        String txt -> do
            let parts = T.splitOn ":" txt
            annotateShow parts
            assert $ length parts == (8 :: Int)
        _unexpectedValue -> assert False

propIPv6ValidHex :: Property
propIPv6ValidHex = property $ do
    value <- forAll genIPv6
    case value of
        String txt -> do
            let parts = T.splitOn ":" txt
            annotateShow parts
            -- Each part should be 4 hex characters
            assert $ all (\p -> T.compareLength p 4 == EQ && T.all isHexChar p) parts
        _unexpectedValue -> assert False
  where
    isHexChar :: Char -> Bool
    isHexChar c = c `elem` ("0123456789abcdef" :: String)

-- Byte (base64) properties

propByteValidBase64 :: Property
propByteValidBase64 = property $ do
    value <- forAll genByte
    case value of
        String txt -> do
            -- Valid base64 should only contain base64 characters
            let validChars = T.all isBase64Char txt
            annotateShow txt
            assert validChars
        _unexpectedValue -> assert False
  where
    isBase64Char :: Char -> Bool
    isBase64Char c =
        c `elem` (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['+', '/', '='])

propByteDecodes :: Property
propByteDecodes = property $ do
    value <- forAll genByte
    case value of
        String txt -> do
            let decoded = Base64.decode (encodeUtf8 txt)
            annotateShow decoded
            case decoded of
                Right _ -> assert True
                Left err -> do
                    annotateShow err
                    assert False
        _unexpectedValue -> assert False

-- URI properties

propURIValidFormat :: Property
propURIValidFormat = property $ do
    value <- forAll genURI
    case value of
        String txt -> do
            annotateShow txt
            -- Should be a valid-looking URI
            assert $ T.isPrefixOf "https://" txt
        _unexpectedValue -> assert False

propURIStartsWithHttps :: Property
propURIStartsWithHttps = property $ do
    value <- forAll genURI
    case value of
        String txt -> assert $ T.isPrefixOf "https://example.com/" txt
        _unexpectedValue -> assert False

-- UUID properties

propUUIDValidFormat :: Property
propUUIDValidFormat = property $ do
    value <- forAll genUUID
    case value of
        String txt -> do
            let parts = T.splitOn "-" txt
            annotateShow parts
            -- UUID has 5 parts: 8-4-4-4-12
            assert $ length parts == (5 :: Int)
        _unexpectedValue -> assert False

propUUIDSegmentLengths :: Property
propUUIDSegmentLengths = property $ do
    value <- forAll genUUID
    case value of
        String txt -> do
            let parts = T.splitOn "-" txt
                expectedLengths = [8, 4, 4, 4, 12]
                actualLengths = map T.length parts
            annotateShow (zip expectedLengths actualLengths)
            assert $ actualLengths == expectedLengths
        _unexpectedValue -> assert False
