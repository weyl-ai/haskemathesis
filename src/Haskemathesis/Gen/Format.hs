{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Gen.Format (
    FormatRegistry,
    defaultFormatRegistry,
    genDate,
    genDateTime,
    genEmail,
    genUUID,
    genURI,
    genIPv4,
    genIPv6,
    genByte,
) where

import Data.Aeson (Value (..))
import qualified Data.ByteString.Base64 as Base64
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type FormatRegistry = Map Text (Gen Value)

defaultFormatRegistry :: FormatRegistry
defaultFormatRegistry =
    Map.fromList
        [ ("date", genDate)
        , ("date-time", genDateTime)
        , ("email", genEmail)
        , ("uuid", genUUID)
        , ("uri", genURI)
        , ("ipv4", genIPv4)
        , ("ipv6", genIPv6)
        , ("byte", genByte)
        ]

genDate :: Gen Value
genDate = do
    year <- Gen.int (Range.linear 1970 2030)
    month <- Gen.int (Range.linear 1 12)
    day <- Gen.int (Range.linear 1 28)
    pure . String . T.pack $
        pad 4 year <> "-" <> pad 2 month <> "-" <> pad 2 day

genDateTime :: Gen Value
genDateTime = do
    year <- Gen.int (Range.linear 1970 2030)
    month <- Gen.int (Range.linear 1 12)
    day <- Gen.int (Range.linear 1 28)
    hour <- Gen.int (Range.linear 0 23)
    minute <- Gen.int (Range.linear 0 59)
    second <- Gen.int (Range.linear 0 59)
    pure . String . T.pack $
        pad 4 year
            <> "-"
            <> pad 2 month
            <> "-"
            <> pad 2 day
            <> "T"
            <> pad 2 hour
            <> ":"
            <> pad 2 minute
            <> ":"
            <> pad 2 second
            <> "Z"

genEmail :: Gen Value
genEmail = do
    user <- Gen.text (Range.linear 1 10) Gen.alphaNum
    pure (String (user <> "@example.com"))

genUUID :: Gen Value
genUUID = do
    a <- hexN 8
    b <- hexN 4
    c <- hexN 4
    d <- hexN 4
    e <- hexN 12
    pure . String $ T.intercalate "-" [a, b, c, d, e]

genURI :: Gen Value
genURI = do
    path <- Gen.text (Range.linear 0 10) Gen.alphaNum
    pure (String ("https://example.com/" <> path))

genIPv4 :: Gen Value
genIPv4 = do
    octets <- Gen.list (Range.singleton 4) (Gen.int (Range.linear 0 255))
    pure . String . T.intercalate "." $ map (T.pack . show) octets

genIPv6 :: Gen Value
genIPv6 = do
    groups <- Gen.list (Range.singleton 8) (hexN 4)
    pure . String $ T.intercalate ":" groups

genByte :: Gen Value
genByte = do
    bytes <- Gen.bytes (Range.linear 0 12)
    pure . String . decodeUtf8 $ Base64.encode bytes

hexN :: Int -> Gen Text
hexN n =
    Gen.text
        (Range.singleton n)
        (Gen.element (['0' .. '9'] <> ['a' .. 'f']))

pad :: Int -> Int -> String
pad width n =
    let s = show n
        zeros = replicate (max 0 (width - length s)) '0'
     in zeros <> s
