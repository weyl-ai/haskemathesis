{-# LANGUAGE OverloadedStrings #-}

{- | Built-in format generators and registry helpers.

This module provides generators for common string formats defined in
OpenAPI and JSON Schema specifications. These generators produce
valid values for formats like dates, emails, UUIDs, and more.

=== Supported Formats

The following formats are supported out of the box:

* @date@ - ISO 8601 date (YYYY-MM-DD)
* @date-time@ - ISO 8601 date-time (YYYY-MM-DDTHH:MM:SSZ)
* @email@ - Email address format
* @uuid@ - UUID v4 format
* @uri@ - URI format
* @ipv4@ - IPv4 address
* @ipv6@ - IPv6 address
* @byte@ - Base64-encoded binary data

=== Using the Format Registry

The 'FormatRegistry' is a map from format names to generators. You can
use the 'defaultFormatRegistry' or create your own with custom formats.

@
import Haskemathesis.Gen.Format (FormatRegistry, defaultFormatRegistry)
import qualified Data.Map.Strict as Map

myRegistry :: FormatRegistry
myRegistry = Map.insert "custom" myCustomGenerator defaultFormatRegistry
@
-}
module Haskemathesis.Gen.Format (
    -- * Format Registry
    FormatRegistry,
    defaultFormatRegistry,

    -- * Individual Format Generators
    genDate,
    genDateTime,
    genEmail,
    genUUID,
    genURI,
    genIPv4,
    genIPv6,
    genByte,
)
where

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
import Text.Printf (printf)

{- | A registry mapping format names to their generators.

This type alias represents a map from format name (e.g., "date", "email")
to a generator that produces 'Value's of that format. You can use
'defaultFormatRegistry' or create your own with custom formats.
-}
type FormatRegistry = Map Text (Gen Value)

{- | The default format registry with built-in format generators.

This registry includes generators for common OpenAPI/JSON Schema formats:

* @date@ - ISO 8601 dates
* @date-time@ - ISO 8601 date-times
* @email@ - Email addresses
* @uuid@ - UUID v4
* @uri@ - URIs
* @ipv4@ - IPv4 addresses
* @ipv6@ - IPv6 addresses
* @byte@ - Base64-encoded data

=== Example

@
import Haskemathesis.Gen.Format (defaultFormatRegistry)
import qualified Data.Map.Strict as Map

-- Look up a format generator
case Map.lookup "date" defaultFormatRegistry of
   Just gen -> ...
   Nothing -> ...
@
-}
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

{- | Generate an ISO 8601 date value (YYYY-MM-DD).

Generates dates between 1970 and 2030 in ISO 8601 format.
The generated values are suitable for JSON Schema @date@ format.

=== Example Output

@
"2023-07-15"
"1999-12-31"
@
-}
genDate :: Gen Value
genDate = do
    year <- Gen.int (Range.linear 1970 2030)
    month <- Gen.int (Range.linear 1 12)
    day <- Gen.int (Range.linear 1 28)
    pure . String . T.pack $
        pad 4 year <> "-" <> pad 2 month <> "-" <> pad 2 day

{- | Generate an ISO 8601 date-time value (YYYY-MM-DDTHH:MM:SSZ).

Generates date-time values in ISO 8601 format with UTC timezone.
The generated values are suitable for JSON Schema @date-time@ format.

=== Example Output

@
"2023-07-15T14:30:45Z"
"1999-12-31T23:59:59Z"
@
-}
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

{- | Generate an email address value.

Generates simple email addresses in the format @local@example.com@.
The generated values are suitable for JSON Schema @email@ format.

=== Example Output

@
"abc123@example.com"
"user@example.com"
@
-}
genEmail :: Gen Value
genEmail = do
    user <- Gen.text (Range.linear 1 10) Gen.alphaNum
    pure (String (user <> "@example.com"))

{- | Generate a UUID v4 value.

Generates UUIDs in the standard 8-4-4-4-12 format.
The generated values are suitable for JSON Schema @uuid@ format.

=== Example Output

@
"550e8400-e29b-41d4-a716-446655440000"
"6ba7b810-9dad-11d1-80b4-00c04fd430c8"
@
-}
genUUID :: Gen Value
genUUID = do
    a <- hexN 8
    b <- hexN 4
    c <- hexN 4
    d <- hexN 4
    e <- hexN 12
    pure . String $ T.intercalate "-" [a, b, c, d, e]

{- | Generate a URI value.

Generates HTTPS URIs with random alphanumeric paths.
The generated values are suitable for JSON Schema @uri@ format.

=== Example Output

@
"https://example.com/abc123"
"https://example.com/path"
@
-}
genURI :: Gen Value
genURI = do
    path <- Gen.text (Range.linear 0 10) Gen.alphaNum
    pure (String ("https://example.com/" <> path))

{- | Generate an IPv4 address value.

Generates valid IPv4 addresses with four octets (0-255).
The generated values are suitable for JSON Schema @ipv4@ format.

=== Example Output

@
"192.168.1.1"
"10.0.0.1"
"255.255.255.0"
@
-}
genIPv4 :: Gen Value
genIPv4 = do
    octets <- Gen.list (Range.singleton 4) (Gen.int (Range.linear 0 255))
    pure . String . T.intercalate "." $ map (T.pack . show) octets

{- | Generate an IPv6 address value.

Generates valid IPv6 addresses with eight groups of four hexadecimal digits.
The generated values are suitable for JSON Schema @ipv6@ format.

=== Example Output

@
"2001:0db8:85a3:0000:0000:8a2e:0370:7334"
"fe80:0000:0000:0000:0000:0000:0000:0001"
@
-}
genIPv6 :: Gen Value
genIPv6 = do
    groups <- Gen.list (Range.singleton 8) (hexN 4)
    pure . String $ T.intercalate ":" groups

{- | Generate a base64-encoded byte value.

Generates random binary data encoded as base64 strings.
The generated values are suitable for JSON Schema @byte@ format.

=== Example Output

@
"SGVsbG8gV29ybGQ="
"QmluYXJ5RGF0YQ=="
@
-}
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
pad width =
    printf ("%0" <> show width <> "d")
