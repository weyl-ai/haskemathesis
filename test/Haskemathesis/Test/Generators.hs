{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Generators (
    genSchema,
) where

import qualified Data.Map.Strict as Map
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Haskemathesis.Schema

genSchema :: Int -> Gen Schema
genSchema depth
    | depth <= 0 = genPrimitiveSchema
    | otherwise =
        Gen.choice
            [ genPrimitiveSchema
            , genArraySchema (depth - 1)
            , genObjectSchema (depth - 1)
            ]

genPrimitiveSchema :: Gen Schema
genPrimitiveSchema =
    Gen.choice
        [ genStringSchema
        , genIntegerSchema
        , genNumberSchema
        , pure emptySchema{schemaType = Just SBoolean}
        ]

genStringSchema :: Gen Schema
genStringSchema = do
    minL <- Gen.int (Range.linear 0 5)
    maxL <- Gen.int (Range.linear minL (minL + 10))
    usePattern <- Gen.bool
    let patternVal = if usePattern then Just "^[a-z0-9]+$" else Nothing
    pure
        emptySchema
            { schemaType = Just SString
            , schemaMinLength = Just minL
            , schemaMaxLength = Just maxL
            , schemaPattern = patternVal
            }

genIntegerSchema :: Gen Schema
genIntegerSchema = do
    lo <- Gen.int (Range.linear (-20) 20)
    hi <- Gen.int (Range.linear lo (lo + 40))
    pure
        emptySchema
            { schemaType = Just SInteger
            , schemaMinimum = Just (fromIntegral lo)
            , schemaMaximum = Just (fromIntegral hi)
            }

genNumberSchema :: Gen Schema
genNumberSchema = do
    lo <- Gen.double (Range.linearFrac (-20) 20)
    hi <- Gen.double (Range.linearFrac lo (lo + 40))
    pure
        emptySchema
            { schemaType = Just SNumber
            , schemaMinimum = Just lo
            , schemaMaximum = Just hi
            }

genArraySchema :: Int -> Gen Schema
genArraySchema depth = do
    minI <- Gen.int (Range.linear 0 3)
    maxI <- Gen.int (Range.linear minI (minI + 5))
    unique <- Gen.bool
    itemSchema <- genSchema depth
    pure
        emptySchema
            { schemaType = Just SArray
            , schemaMinItems = Just minI
            , schemaMaxItems = Just maxI
            , schemaUniqueItems = unique
            , schemaItems = Just itemSchema
            }

genObjectSchema :: Int -> Gen Schema
genObjectSchema _depth = do
    keyCount <- Gen.int (Range.linear 0 3)
    keys <- Gen.list (Range.singleton keyCount) (Gen.text (Range.linear 1 5) Gen.alphaNum)
    let uniqueKeys = Map.fromList [(k, ()) | k <- keys]
        props =
            Map.fromList
                [ (k, emptySchema{schemaType = Just SString})
                | k <- Map.keys uniqueKeys
                ]
    requiredCount <- Gen.int (Range.linear 0 (Map.size props))
    requiredKeys <-
        Gen.subsequence (Map.keys props)
    let required = take requiredCount requiredKeys
    pure
        emptySchema
            { schemaType = Just SObject
            , schemaProperties = props
            , schemaRequired = required
            }
