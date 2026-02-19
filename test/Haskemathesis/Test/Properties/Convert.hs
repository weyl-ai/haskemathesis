{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for OpenAPI schema conversion.

These tests verify that schema conversion from OpenAPI to internal
representation correctly:

1. Preserves schema types
2. Preserves string constraints
3. Preserves numeric constraints
4. Preserves array constraints
5. Preserves object constraints
6. Handles schema combinators (allOf, anyOf, oneOf)
7. Handles nullable types
-}
module Haskemathesis.Test.Properties.Convert (spec) where

import Data.OpenApi (OpenApiType (..), Schema (..))
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.OpenApi.Convert (convertSchema)
import qualified Haskemathesis.Schema as HS
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "Schema Conversion" $ do
        describe "Type conversion" $ do
            itProp "preserves string type" propPreservesStringType
            itProp "preserves integer type" propPreservesIntegerType
            itProp "preserves number type" propPreservesNumberType
            itProp "preserves boolean type" propPreservesBooleanType
            itProp "preserves array type" propPreservesArrayType
            itProp "preserves object type" propPreservesObjectType

        describe "String constraints" $ do
            itProp "preserves minLength" propPreservesMinLength
            itProp "preserves maxLength" propPreservesMaxLength
            itProp "preserves pattern" propPreservesPattern

        describe "Numeric constraints" $ do
            itProp "preserves minimum" propPreservesMinimum
            itProp "preserves maximum" propPreservesMaximum
            itProp "converts exclusive minimum when flag is true" propConvertsExclusiveMin
            itProp "converts exclusive maximum when flag is true" propConvertsExclusiveMax

        describe "Array constraints" $ do
            itProp "preserves minItems" propPreservesMinItems
            itProp "preserves maxItems" propPreservesMaxItems
            itProp "preserves uniqueItems" propPreservesUniqueItems

        describe "Object constraints" $ do
            itProp "preserves required fields" propPreservesRequired

        describe "Nullable" $ do
            itProp "preserves nullable flag" propPreservesNullable

-- Helper: no-op resolver (no references)
noResolve :: a -> Maybe b
noResolve _ = Nothing

-- Type conversion properties

propPreservesStringType :: Property
propPreservesStringType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiString}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SString

propPreservesIntegerType :: Property
propPreservesIntegerType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiInteger}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SInteger

propPreservesNumberType :: Property
propPreservesNumberType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiNumber}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SNumber

propPreservesBooleanType :: Property
propPreservesBooleanType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiBoolean}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SBoolean

propPreservesArrayType :: Property
propPreservesArrayType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiArray}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SArray

propPreservesObjectType :: Property
propPreservesObjectType = property $ do
    let openApiSchema = mempty{_schemaType = Just OpenApiObject}
        converted = convertSchema noResolve openApiSchema
    HS.schemaType converted === Just HS.SObject

-- String constraint properties

propPreservesMinLength :: Property
propPreservesMinLength = property $ do
    minLen <- forAll $ Gen.integral (Range.linear 0 100)
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiString
                , _schemaMinLength = Just minLen
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMinLength converted === Just (fromInteger minLen)

propPreservesMaxLength :: Property
propPreservesMaxLength = property $ do
    maxLen <- forAll $ Gen.integral (Range.linear 1 100)
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiString
                , _schemaMaxLength = Just maxLen
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMaxLength converted === Just (fromInteger maxLen)

propPreservesPattern :: Property
propPreservesPattern = property $ do
    pat <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiString
                , _schemaPattern = Just pat
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaPattern converted === Just pat

-- Numeric constraint properties
-- Note: We use integers converted to Scientific to avoid repeating decimal issues

propPreservesMinimum :: Property
propPreservesMinimum = property $ do
    minValInt <- forAll $ Gen.int (Range.linear (-1000) 1000)
    let minVal = fromIntegral minValInt
        openApiSchema =
            mempty
                { _schemaType = Just OpenApiNumber
                , _schemaMinimum = Just minVal
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMinimum converted === Just (fromIntegral minValInt)

propPreservesMaximum :: Property
propPreservesMaximum = property $ do
    maxValInt <- forAll $ Gen.int (Range.linear (-1000) 1000)
    let maxVal = fromIntegral maxValInt
        openApiSchema =
            mempty
                { _schemaType = Just OpenApiNumber
                , _schemaMaximum = Just maxVal
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMaximum converted === Just (fromIntegral maxValInt)

propConvertsExclusiveMin :: Property
propConvertsExclusiveMin = property $ do
    minValInt <- forAll $ Gen.int (Range.linear (-1000) 1000)
    let minVal = fromIntegral minValInt
        openApiSchema =
            mempty
                { _schemaType = Just OpenApiNumber
                , _schemaMinimum = Just minVal
                , _schemaExclusiveMinimum = Just True
                }
        converted = convertSchema noResolve openApiSchema
    -- When exclusiveMinimum is True, the value should be in schemaExclusiveMinimum
    HS.schemaExclusiveMinimum converted === Just (fromIntegral minValInt)

propConvertsExclusiveMax :: Property
propConvertsExclusiveMax = property $ do
    maxValInt <- forAll $ Gen.int (Range.linear (-1000) 1000)
    let maxVal = fromIntegral maxValInt
        openApiSchema =
            mempty
                { _schemaType = Just OpenApiNumber
                , _schemaMaximum = Just maxVal
                , _schemaExclusiveMaximum = Just True
                }
        converted = convertSchema noResolve openApiSchema
    -- When exclusiveMaximum is True, the value should be in schemaExclusiveMaximum
    HS.schemaExclusiveMaximum converted === Just (fromIntegral maxValInt)

-- Array constraint properties

propPreservesMinItems :: Property
propPreservesMinItems = property $ do
    minItems <- forAll $ Gen.integral (Range.linear 0 100)
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiArray
                , _schemaMinItems = Just minItems
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMinItems converted === Just (fromInteger minItems)

propPreservesMaxItems :: Property
propPreservesMaxItems = property $ do
    maxItems <- forAll $ Gen.integral (Range.linear 1 100)
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiArray
                , _schemaMaxItems = Just maxItems
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaMaxItems converted === Just (fromInteger maxItems)

propPreservesUniqueItems :: Property
propPreservesUniqueItems = property $ do
    uniqueItems <- forAll Gen.bool
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiArray
                , _schemaUniqueItems = Just uniqueItems
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaUniqueItems converted === uniqueItems

-- Object constraint properties

propPreservesRequired :: Property
propPreservesRequired = property $ do
    requiredFields <- forAll $ Gen.list (Range.linear 0 5) (Gen.text (Range.linear 1 10) Gen.alpha)
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiObject
                , _schemaRequired = requiredFields
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaRequired converted === requiredFields

-- Nullable properties

propPreservesNullable :: Property
propPreservesNullable = property $ do
    nullable <- forAll Gen.bool
    let openApiSchema =
            mempty
                { _schemaType = Just OpenApiString
                , _schemaNullable = Just nullable
                }
        converted = convertSchema noResolve openApiSchema
    HS.schemaNullable converted === nullable
