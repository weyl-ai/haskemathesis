{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Loader (spec) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V
import Haskemathesis.OpenApi.Loader (isUrl, transformOpenApi31To30)
import Haskemathesis.Test.Support (itProp)
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "OpenApi.Loader" $ do
    describe "transformOpenApi31To30" $ do
        itProp "transforms OpenAPI version 3.1.x to 3.0.3" propVersionTransform
        itProp "preserves OpenAPI version 3.0.x unchanged" propVersion30Unchanged
        itProp "transforms numeric exclusiveMinimum to minimum + boolean" propExclusiveMinimumTransform
        itProp "transforms numeric exclusiveMaximum to maximum + boolean" propExclusiveMaximumTransform
        itProp "preserves boolean exclusiveMinimum unchanged" propBooleanExclusiveMinimumUnchanged
        itProp "preserves boolean exclusiveMaximum unchanged" propBooleanExclusiveMaximumUnchanged
        itProp "transforms nested schemas recursively" propNestedTransform
        itProp "transforms arrays of schemas" propArrayTransform
        itProp "preserves non-schema objects unchanged" propNonSchemaUnchanged
        itProp "handles both exclusiveMinimum and exclusiveMaximum together" propBothExclusivesTransform

    describe "isUrl" $ do
        itProp "detects http URLs" propDetectsHttp
        itProp "detects https URLs" propDetectsHttps
        itProp "rejects file paths" propRejectsFilePaths

-- | OpenAPI version 3.1.x should be transformed to 3.0.3
propVersionTransform :: Property
propVersionTransform = property $ do
    minor <- forAll $ Gen.int (Range.linear 0 9)
    let version = String $ "3.1." <> T.pack (show minor)
        input =
            Object $
                KM.fromList
                    [ ("openapi", version)
                    , ("info", Object $ KM.fromList [("title", String "Test")])
                    ]
        expected =
            Object $
                KM.fromList
                    [ ("openapi", String "3.0.3")
                    , ("info", Object $ KM.fromList [("title", String "Test")])
                    ]
    transformOpenApi31To30 input === expected

-- | OpenAPI version 3.0.x should be preserved unchanged
propVersion30Unchanged :: Property
propVersion30Unchanged = property $ do
    minor <- forAll $ Gen.int (Range.linear 0 3)
    let version = "3.0." <> T.pack (show minor)
        input =
            Object $
                KM.fromList
                    [ ("openapi", String version)
                    , ("info", Object $ KM.fromList [("title", String "Test")])
                    ]
    transformOpenApi31To30 input === input

-- | Numeric exclusiveMinimum should become minimum + exclusiveMinimum: true
propExclusiveMinimumTransform :: Property
propExclusiveMinimumTransform = property $ do
    n <- forAll $ Gen.integral (Range.linear (-1000) 1000 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("exclusiveMinimum", Number (fromIntegral n))
                    ]
        expected =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("minimum", Number (fromIntegral n))
                    , ("exclusiveMinimum", Bool True)
                    ]
    transformOpenApi31To30 input === expected

-- | Numeric exclusiveMaximum should become maximum + exclusiveMaximum: true
propExclusiveMaximumTransform :: Property
propExclusiveMaximumTransform = property $ do
    n <- forAll $ Gen.integral (Range.linear (-1000) 1000 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("exclusiveMaximum", Number (fromIntegral n))
                    ]
        expected =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("maximum", Number (fromIntegral n))
                    , ("exclusiveMaximum", Bool True)
                    ]
    transformOpenApi31To30 input === expected

-- | Boolean exclusiveMinimum should be preserved (already 3.0 format)
propBooleanExclusiveMinimumUnchanged :: Property
propBooleanExclusiveMinimumUnchanged = property $ do
    b <- forAll Gen.bool
    n <- forAll $ Gen.integral (Range.linear (-1000) 1000 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("minimum", Number (fromIntegral n))
                    , ("exclusiveMinimum", Bool b)
                    ]
    -- Should be unchanged since exclusiveMinimum is already boolean
    transformOpenApi31To30 input === input

-- | Boolean exclusiveMaximum should be preserved (already 3.0 format)
propBooleanExclusiveMaximumUnchanged :: Property
propBooleanExclusiveMaximumUnchanged = property $ do
    b <- forAll Gen.bool
    n <- forAll $ Gen.integral (Range.linear (-1000) 1000 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("maximum", Number (fromIntegral n))
                    , ("exclusiveMaximum", Bool b)
                    ]
    -- Should be unchanged since exclusiveMaximum is already boolean
    transformOpenApi31To30 input === input

-- | Nested schemas should be transformed recursively
propNestedTransform :: Property
propNestedTransform = property $ do
    n <- forAll $ Gen.integral (Range.linear 0 100 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [
                        ( "properties"
                        , Object $
                            KM.fromList
                                [
                                    ( "age"
                                    , Object $
                                        KM.fromList
                                            [ ("type", String "integer")
                                            , ("exclusiveMinimum", Number (fromIntegral n))
                                            ]
                                    )
                                ]
                        )
                    ]
        expected =
            Object $
                KM.fromList
                    [
                        ( "properties"
                        , Object $
                            KM.fromList
                                [
                                    ( "age"
                                    , Object $
                                        KM.fromList
                                            [ ("type", String "integer")
                                            , ("minimum", Number (fromIntegral n))
                                            , ("exclusiveMinimum", Bool True)
                                            ]
                                    )
                                ]
                        )
                    ]
    transformOpenApi31To30 input === expected

-- | Arrays containing schemas should be transformed
propArrayTransform :: Property
propArrayTransform = property $ do
    n <- forAll $ Gen.integral (Range.linear 0 100 :: Range.Range Integer)
    let schema =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("exclusiveMinimum", Number (fromIntegral n))
                    ]
        input = Array (V.fromList [schema, String "other", Number 42])
        expectedSchema =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("minimum", Number (fromIntegral n))
                    , ("exclusiveMinimum", Bool True)
                    ]
        expected = Array (V.fromList [expectedSchema, String "other", Number 42])
    transformOpenApi31To30 input === expected

-- | Objects without exclusiveMinimum/Maximum should be unchanged
propNonSchemaUnchanged :: Property
propNonSchemaUnchanged = property $ do
    name <- forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
    let input =
            Object $
                KM.fromList
                    [ ("name", String name)
                    , ("version", String "1.0.0")
                    ]
    transformOpenApi31To30 input === input

-- | Both exclusiveMinimum and exclusiveMaximum in same object
propBothExclusivesTransform :: Property
propBothExclusivesTransform = property $ do
    minVal <- forAll $ Gen.integral (Range.linear 0 50 :: Range.Range Integer)
    maxVal <- forAll $ Gen.integral (Range.linear 51 100 :: Range.Range Integer)
    let input =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("exclusiveMinimum", Number (fromIntegral minVal))
                    , ("exclusiveMaximum", Number (fromIntegral maxVal))
                    ]
        expected =
            Object $
                KM.fromList
                    [ ("type", String "integer")
                    , ("minimum", Number (fromIntegral minVal))
                    , ("exclusiveMinimum", Bool True)
                    , ("maximum", Number (fromIntegral maxVal))
                    , ("exclusiveMaximum", Bool True)
                    ]
    transformOpenApi31To30 input === expected

-- | http:// URLs are detected
propDetectsHttp :: Property
propDetectsHttp = property $ do
    path <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let url = "http://example.com/" <> T.unpack path
    isUrl url === True

-- | https:// URLs are detected
propDetectsHttps :: Property
propDetectsHttps = property $ do
    path <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
    let url = "https://example.com/" <> T.unpack path
    isUrl url === True

-- | File paths are not detected as URLs
propRejectsFilePaths :: Property
propRejectsFilePaths = property $ do
    path <- forAll $ Gen.element ["api.yaml", "./spec.json", "/path/to/spec.yaml", "specs/api.json"]
    isUrl path === False
