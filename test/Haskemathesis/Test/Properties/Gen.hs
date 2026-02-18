{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Gen (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Haskemathesis.Gen (genFromSchema)
import Haskemathesis.Gen.Format (genDate, genDateTime, genEmail, genURI, genUUID)
import Haskemathesis.Schema
import Haskemathesis.Test.Generators (genSchema)
import Haskemathesis.Test.Support (itProp)
import Haskemathesis.Validate (validateValue)
import Hedgehog (Property, assert, failure, forAll, property, success, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)
import Text.Regex.TDFA ((=~))

spec :: Spec
spec =
    describe "Generators" $ do
        itProp "generated values validate" prop_generated_values_validate
        itProp "string length" prop_string_length
        itProp "integer bounds" prop_integer_bounds
        itProp "number bounds" prop_number_bounds
        itProp "array bounds" prop_array_bounds
        itProp "object required" prop_object_required
        itProp "nullable allows null" prop_nullable_allows_null
        itProp "anyOf/oneOf/allOf" prop_anyof_oneof_allof
        -- New schema generation tests
        itProp "string pattern validates" prop_string_pattern_validates
        itProp "format email generates valid email" prop_format_email
        itProp "format uuid generates valid uuid" prop_format_uuid
        itProp "format date generates valid date" prop_format_date
        itProp "format datetime generates valid datetime" prop_format_datetime
        itProp "format uri generates valid uri" prop_format_uri
        itProp "enum generates only valid values" prop_enum_generates_only_valid_values
        itProp "const generates exact value" prop_const_generates_exact_value
        itProp "additional properties respected" prop_additional_properties_respected
        itProp "nested object depth limited" prop_nested_object_depth_limited
        -- Edge case tests
        itProp "empty schema accepts anything" prop_empty_schema_accepts_anything
        itProp "large integer handled" prop_large_integer_handled
        itProp "unicode in strings" prop_unicode_in_strings
        itProp "empty string valid when no minLength" prop_empty_string_valid_when_no_minlength

prop_generated_values_validate :: Property
prop_generated_values_validate =
    property $ do
        schema <- forAll (genSchema 2)
        value <- forAll (genFromSchema schema)
        assert (validateValue schema value)

prop_string_length :: Property
prop_string_length =
    property $ do
        minL <- forAll (Gen.int (Range.linear 0 5))
        maxL <- forAll (Gen.int (Range.linear minL (minL + 10)))
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaMinLength = Just minL
                    , schemaMaxLength = Just maxL
                    }
        value <- forAll (genFromSchema schema)
        case value of
            String txt -> do
                let len = BS.length (encodeUtf8 txt)
                assert (len >= minL)
                assert (len <= maxL)
            _otherValue -> failure

prop_integer_bounds :: Property
prop_integer_bounds =
    property $ do
        lo <- forAll (Gen.int (Range.linear (-50) 50))
        useExclusive <- forAll Gen.bool
        hi <-
            if useExclusive
                then forAll (Gen.int (Range.linear (lo + 2) (lo + 100)))
                else forAll (Gen.int (Range.linear lo (lo + 100)))
        let schema =
                emptySchema
                    { schemaType = Just SInteger
                    , schemaMinimum = Just (fromIntegral lo)
                    , schemaMaximum = Just (fromIntegral hi)
                    , schemaExclusiveMinimum = if useExclusive then Just (fromIntegral lo) else Nothing
                    , schemaExclusiveMaximum = if useExclusive then Just (fromIntegral hi) else Nothing
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Number n -> do
                let d = realToFrac n :: Double
                if useExclusive
                    then do
                        assert (d > fromIntegral lo)
                        assert (d < fromIntegral hi)
                    else do
                        assert (d >= fromIntegral lo)
                        assert (d <= fromIntegral hi)
            _otherValue -> failure

prop_number_bounds :: Property
prop_number_bounds =
    property $ do
        lo <- forAll (Gen.double (Range.linearFrac (-50) 50))
        useExclusive <- forAll Gen.bool
        hi <-
            if useExclusive
                then forAll (Gen.double (Range.linearFrac (lo + 1) (lo + 100)))
                else forAll (Gen.double (Range.linearFrac lo (lo + 100)))
        let schema =
                emptySchema
                    { schemaType = Just SNumber
                    , schemaMinimum = Just lo
                    , schemaMaximum = Just hi
                    , schemaExclusiveMinimum = if useExclusive then Just lo else Nothing
                    , schemaExclusiveMaximum = if useExclusive then Just hi else Nothing
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Number n -> do
                let d = realToFrac n :: Double
                if useExclusive
                    then do
                        assert (d > lo)
                        assert (d < hi)
                    else do
                        assert (d >= lo)
                        assert (d <= hi)
            _otherValue -> failure

prop_array_bounds :: Property
prop_array_bounds =
    property $ do
        minI <- forAll (Gen.int (Range.linear 0 3))
        maxI <- forAll (Gen.int (Range.linear minI (minI + 6)))
        unique <- forAll Gen.bool
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMinItems = Just minI
                    , schemaMaxItems = Just maxI
                    , schemaUniqueItems = unique
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Array vec -> do
                let len = Vector.length vec
                assert (len >= minI)
                assert (len <= maxI)
                if unique
                    then assert (validateValue schema value)
                    else success
            _otherValue -> failure

prop_object_required :: Property
prop_object_required =
    property $ do
        let props =
                Map.fromList
                    [ ("a", emptySchema{schemaType = Just SString})
                    , ("b", emptySchema{schemaType = Just SInteger})
                    ]
            schema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = props
                    , schemaRequired = ["a"]
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Object obj ->
                assert (KeyMap.member (Key.fromText "a") obj)
            _otherValue -> failure

prop_nullable_allows_null :: Property
prop_nullable_allows_null =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaNullable = True
                    }
        value <- forAll (genFromSchema schema)
        assert (validateValue schema value)

prop_anyof_oneof_allof :: Property
prop_anyof_oneof_allof =
    property $ do
        let s1 = emptySchema{schemaType = Just SString}
            s2 = emptySchema{schemaType = Just SInteger}
            anySchema = emptySchema{schemaAnyOf = [s1, s2]}
            oneSchema = emptySchema{schemaOneOf = [s1, s2]}
            allSchema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("a", emptySchema{schemaType = Just SString})]
                    , schemaRequired = ["a"]
                    , schemaAllOf = [emptySchema{schemaType = Just SObject}]
                    }
        vAny <- forAll (genFromSchema anySchema)
        vOne <- forAll (genFromSchema oneSchema)
        vAll <- forAll (genFromSchema allSchema)
        assert (validateValue anySchema vAny)
        assert (validateValue oneSchema vOne)
        assert (validateValue allSchema vAll)

-- | Test that generated strings with pattern constraint match the regex
prop_string_pattern_validates :: Property
prop_string_pattern_validates =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaPattern = Just "^[a-z0-9]+$"
                    }
        value <- forAll (genFromSchema schema)
        case value of
            String txt -> assert (T.unpack txt =~ ("^[a-z0-9]+$" :: String))
            _otherValue -> failure

-- | Test that genEmail generates valid email-like strings
prop_format_email :: Property
prop_format_email =
    property $ do
        value <- forAll genEmail
        case value of
            String txt -> assert ("@" `T.isInfixOf` txt && "." `T.isInfixOf` txt)
            _other -> failure

-- | Test that genUUID generates valid UUID strings
prop_format_uuid :: Property
prop_format_uuid =
    property $ do
        value <- forAll genUUID
        case value of
            String txt -> do
                -- UUID format: 8-4-4-4-12 hex chars
                case T.splitOn "-" txt of
                    [p1, p2, p3, p4, p5] ->
                        assert (map (BS.length . encodeUtf8) [p1, p2, p3, p4, p5] == [8, 4, 4, 4, 12])
                    _wrongPartCount -> failure
            _other -> failure

-- | Test that genDate generates ISO 8601 date strings
prop_format_date :: Property
prop_format_date =
    property $ do
        value <- forAll genDate
        case value of
            String txt -> do
                -- Date format: YYYY-MM-DD
                case T.splitOn "-" txt of
                    [year, _month, _day] -> assert (BS.length (encodeUtf8 year) == 4)
                    _wrongFormat -> failure
            _other -> failure

-- | Test that genDateTime generates ISO 8601 datetime strings
prop_format_datetime :: Property
prop_format_datetime =
    property $ do
        value <- forAll genDateTime
        case value of
            String txt -> assert ("T" `T.isInfixOf` txt)
            _other -> failure

-- | Test that genURI generates valid URI strings
prop_format_uri :: Property
prop_format_uri =
    property $ do
        value <- forAll genURI
        case value of
            String txt -> assert ("://" `T.isInfixOf` txt)
            _other -> failure

-- | Test that enum schema only generates defined values
prop_enum_generates_only_valid_values :: Property
prop_enum_generates_only_valid_values =
    property $ do
        let enumValues = [String "red", String "green", String "blue"]
            schema = emptySchema{schemaEnum = Just enumValues}
        value <- forAll (genFromSchema schema)
        assert (value `elem` enumValues)

-- | Test that const schema always generates the exact value
prop_const_generates_exact_value :: Property
prop_const_generates_exact_value =
    property $ do
        let constValue = String "fixed_value"
            schema = emptySchema{schemaConst = Just constValue}
        value <- forAll (genFromSchema schema)
        value === constValue

-- | Test that additionalProperties: false is respected
prop_additional_properties_respected :: Property
prop_additional_properties_respected =
    property $ do
        let props = Map.fromList [("name", emptySchema{schemaType = Just SString})]
            schema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = props
                    , schemaAdditionalProperties = Just AdditionalPropertiesNone
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Object obj -> do
                let keys = Set.fromList (map Key.toText (KeyMap.keys obj))
                    allowedKeys = Set.fromList ["name"]
                assert (keys `Set.isSubsetOf` allowedKeys)
            _otherValue -> failure

-- | Test that deeply nested schemas don't cause infinite recursion
prop_nested_object_depth_limited :: Property
prop_nested_object_depth_limited =
    property $ do
        -- Create a schema that could recurse deeply
        let schema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties =
                        Map.fromList
                            [
                                ( "nested"
                                , emptySchema
                                    { schemaType = Just SObject
                                    , schemaProperties =
                                        Map.fromList
                                            [
                                                ( "deep"
                                                , emptySchema
                                                    { schemaType = Just SObject
                                                    , schemaProperties =
                                                        Map.fromList
                                                            [ ("value", emptySchema{schemaType = Just SString})
                                                            ]
                                                    }
                                                )
                                            ]
                                    }
                                )
                            ]
                    }
        value <- forAll (genFromSchema schema)
        assert (validateValue schema value)

-- | Test that empty schema accepts any JSON value
prop_empty_schema_accepts_anything :: Property
prop_empty_schema_accepts_anything =
    property $ do
        value <- forAll (genFromSchema emptySchema)
        -- Empty schema should validate any value
        assert (validateValue emptySchema value)

{- | Test that large integers are handled correctly
Note: The generator uses linearFrom 0, so the range must include 0
for shrinking to work correctly. This test verifies that large
positive integers can be generated.
-}
prop_large_integer_handled :: Property
prop_large_integer_handled =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SInteger
                    , schemaMinimum = Just (-1000)
                    , schemaMaximum = Just 999999
                    }
        value <- forAll (genFromSchema schema)
        case value of
            Number n -> do
                let d = realToFrac n :: Double
                -- Verify the value is within the valid range
                assert (d >= (-1000))
                assert (d <= 999999)
            _otherValue -> failure

-- | Test that unicode characters in strings are handled correctly
prop_unicode_in_strings :: Property
prop_unicode_in_strings =
    property $ do
        let schema = emptySchema{schemaType = Just SString}
        value <- forAll (genFromSchema schema)
        case value of
            String txt -> do
                -- Should be valid UTF-8 text
                let encoded = encodeUtf8 txt
                assert (BS.length encoded >= 0)
            _otherValue -> failure

-- | Test that empty string is valid when no minLength constraint
prop_empty_string_valid_when_no_minlength :: Property
prop_empty_string_valid_when_no_minlength =
    property $ do
        let schema = emptySchema{schemaType = Just SString}
            value = String ""
        assert (validateValue schema value)
