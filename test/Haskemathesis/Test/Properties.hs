{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties (
    tests,
) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Haskemathesis.Gen
import Haskemathesis.Schema
import Haskemathesis.Test.Generators (genSchema)
import Haskemathesis.Validate

tests :: Group
tests =
    Group
        "haskemathesis"
        [ ("prop_generated_values_validate", prop_generated_values_validate)
        , ("prop_string_length", prop_string_length)
        , ("prop_integer_bounds", prop_integer_bounds)
        , ("prop_number_bounds", prop_number_bounds)
        , ("prop_array_bounds", prop_array_bounds)
        , ("prop_object_required", prop_object_required)
        , ("prop_nullable_allows_null", prop_nullable_allows_null)
        , ("prop_anyof_oneof_allof", prop_anyof_oneof_allof)
        ]

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
                assert (T.length txt >= minL)
                assert (T.length txt <= maxL)
            _ -> failure

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
            _ -> failure

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
            _ -> failure

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
            _ -> failure

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
            _ -> failure

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
