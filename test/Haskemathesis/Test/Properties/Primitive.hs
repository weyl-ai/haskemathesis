{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for primitive generators.

These tests verify that primitive generators correctly:

1. Respect schema constraints (min/max length, numeric bounds)
2. Handle const and enum values
3. Generate values within specified ranges
-}
module Haskemathesis.Test.Properties.Primitive (spec) where

import Data.Aeson (Value (..))
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Hedgehog (Property, annotateShow, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.Gen.Primitive (genBoolean, genConstOrEnum, genInteger, genNumber, genString)
import Haskemathesis.Schema (Schema (..), SchemaType (..), emptySchema)
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "Primitive Generators" $ do
        describe "genString" $ do
            itProp "respects minLength constraint" propStringMinLength
            itProp "respects maxLength constraint" propStringMaxLength
            itProp "respects both min and max length" propStringBothLengths

        describe "genInteger" $ do
            itProp "respects minimum constraint" propIntegerMinimum
            itProp "respects maximum constraint" propIntegerMaximum
            itProp "respects exclusive minimum" propIntegerExclusiveMin
            itProp "respects exclusive maximum" propIntegerExclusiveMax

        describe "genNumber" $ do
            itProp "respects minimum constraint" propNumberMinimum
            itProp "respects maximum constraint" propNumberMaximum
            itProp "respects exclusive minimum" propNumberExclusiveMin
            itProp "respects exclusive maximum" propNumberExclusiveMax

        describe "genBoolean" $ do
            itProp "generates valid booleans" propBooleanValid

        describe "genConstOrEnum" $ do
            itProp "prefers const over fallback" propConstPreferred
            itProp "prefers enum over fallback" propEnumPreferred
            itProp "uses fallback when no const or enum" propFallbackUsed

-- String properties

propStringMinLength :: Property
propStringMinLength = property $ do
    minLen <- forAll $ Gen.int (Range.linear 1 20)
    let schema = emptySchema{schemaType = Just SString, schemaMinLength = Just minLen}
    value <- forAll $ genString schema
    case value of
        String txt -> assert $ T.compareLength txt minLen /= LT
        _unexpectedValue -> assert False

propStringMaxLength :: Property
propStringMaxLength = property $ do
    maxLen <- forAll $ Gen.int (Range.linear 5 50)
    let schema = emptySchema{schemaType = Just SString, schemaMaxLength = Just maxLen}
    value <- forAll $ genString schema
    case value of
        String txt -> assert $ T.compareLength txt maxLen /= GT
        _unexpectedValue -> assert False

propStringBothLengths :: Property
propStringBothLengths = property $ do
    minLen <- forAll $ Gen.int (Range.linear 1 10)
    maxLen <- forAll $ Gen.int (Range.linear minLen (minLen + 20))
    let schema =
            emptySchema
                { schemaType = Just SString
                , schemaMinLength = Just minLen
                , schemaMaxLength = Just maxLen
                }
    value <- forAll $ genString schema
    case value of
        String txt -> do
            annotateShow txt
            assert $ T.compareLength txt minLen /= LT && T.compareLength txt maxLen /= GT
        _unexpectedValue -> assert False

-- Integer properties

propIntegerMinimum :: Property
propIntegerMinimum = property $ do
    minVal <- forAll $ Gen.int (Range.linear (-500) 500)
    let schema = emptySchema{schemaType = Just SInteger, schemaMinimum = Just (fromIntegral minVal)}
    value <- forAll $ genInteger schema
    case value of
        Number n -> assert $ (toRealFloat n :: Double) >= fromIntegral minVal
        _unexpectedValue -> assert False

propIntegerMaximum :: Property
propIntegerMaximum = property $ do
    maxVal <- forAll $ Gen.int (Range.linear (-500) 500)
    let schema = emptySchema{schemaType = Just SInteger, schemaMaximum = Just (fromIntegral maxVal)}
    value <- forAll $ genInteger schema
    case value of
        Number n -> assert $ (toRealFloat n :: Double) <= fromIntegral maxVal
        _unexpectedValue -> assert False

propIntegerExclusiveMin :: Property
propIntegerExclusiveMin = property $ do
    minVal <- forAll $ Gen.int (Range.linear (-500) 500)
    let schema = emptySchema{schemaType = Just SInteger, schemaExclusiveMinimum = Just (fromIntegral minVal)}
    value <- forAll $ genInteger schema
    case value of
        Number n -> assert $ (toRealFloat n :: Double) > fromIntegral minVal
        _unexpectedValue -> assert False

propIntegerExclusiveMax :: Property
propIntegerExclusiveMax = property $ do
    maxVal <- forAll $ Gen.int (Range.linear (-500) 500)
    let schema = emptySchema{schemaType = Just SInteger, schemaExclusiveMaximum = Just (fromIntegral maxVal)}
    value <- forAll $ genInteger schema
    case value of
        Number n -> assert $ (toRealFloat n :: Double) < fromIntegral maxVal
        _unexpectedValue -> assert False

-- Number properties

propNumberMinimum :: Property
propNumberMinimum = property $ do
    minVal <- forAll $ Gen.double (Range.linearFrac (-500) 500)
    let schema = emptySchema{schemaType = Just SNumber, schemaMinimum = Just minVal}
    value <- forAll $ genNumber schema
    case value of
        Number n -> assert $ toRealFloat n >= minVal
        _unexpectedValue -> assert False

propNumberMaximum :: Property
propNumberMaximum = property $ do
    maxVal <- forAll $ Gen.double (Range.linearFrac (-500) 500)
    let schema = emptySchema{schemaType = Just SNumber, schemaMaximum = Just maxVal}
    value <- forAll $ genNumber schema
    case value of
        Number n -> assert $ toRealFloat n <= maxVal
        _unexpectedValue -> assert False

propNumberExclusiveMin :: Property
propNumberExclusiveMin = property $ do
    minVal <- forAll $ Gen.double (Range.linearFrac (-500) 500)
    let schema = emptySchema{schemaType = Just SNumber, schemaExclusiveMinimum = Just minVal}
    value <- forAll $ genNumber schema
    case value of
        Number n -> assert $ toRealFloat n > minVal
        _unexpectedValue -> assert False

propNumberExclusiveMax :: Property
propNumberExclusiveMax = property $ do
    maxVal <- forAll $ Gen.double (Range.linearFrac (-500) 500)
    let schema = emptySchema{schemaType = Just SNumber, schemaExclusiveMaximum = Just maxVal}
    value <- forAll $ genNumber schema
    case value of
        Number n -> assert $ toRealFloat n < maxVal
        _unexpectedValue -> assert False

-- Boolean properties

propBooleanValid :: Property
propBooleanValid = property $ do
    let schema = emptySchema{schemaType = Just SBoolean}
    value <- forAll $ genBoolean schema
    case value of
        Bool _ -> assert True
        _unexpectedValue -> assert False

-- ConstOrEnum properties

propConstPreferred :: Property
propConstPreferred = property $ do
    constVal <- forAll $ Gen.element [String "const1", String "const2", Number 42]
    let schema = emptySchema{schemaConst = Just constVal}
        fallback = pure (String "fallback")
    value <- forAll $ genConstOrEnum schema fallback
    assert $ value == constVal

propEnumPreferred :: Property
propEnumPreferred = property $ do
    enumVals <- forAll $ Gen.list (Range.linear 2 5) (String <$> Gen.text (Range.linear 1 10) Gen.alpha)
    let schema = emptySchema{schemaEnum = Just enumVals}
        fallback = pure (String "fallback")
    value <- forAll $ genConstOrEnum schema fallback
    assert $ value `elem` enumVals

propFallbackUsed :: Property
propFallbackUsed = property $ do
    let schema = emptySchema -- no const or enum
        fallbackVal = String "fallback"
        fallback = pure fallbackVal
    value <- forAll $ genConstOrEnum schema fallback
    assert $ value == fallbackVal
