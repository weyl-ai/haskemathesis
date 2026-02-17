{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Validate (spec) where

import Data.Aeson (Value (..))
import qualified Data.Vector as Vector
import Hedgehog (Property, assert, property)
import Test.Hspec (Spec, describe)

import Haskemathesis.Schema
import Haskemathesis.Test.Support (itProp)
import Haskemathesis.Validate (validateValue)

spec :: Spec
spec =
    describe "Validation" $ do
        itProp "unique items violation" prop_validate_unique_items_violation
        itProp "min items violation" prop_validate_min_items_violation
        itProp "max items violation" prop_validate_max_items_violation
        itProp "string length violation" prop_validate_string_length_violation

prop_validate_unique_items_violation :: Property
prop_validate_unique_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaUniqueItems = True
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1, Number 1])
        assert (not (validateValue schema value))

prop_validate_min_items_violation :: Property
prop_validate_min_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMinItems = Just 2
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1])
        assert (not (validateValue schema value))

prop_validate_max_items_violation :: Property
prop_validate_max_items_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SArray
                    , schemaMaxItems = Just 1
                    , schemaItems = Just (emptySchema{schemaType = Just SInteger})
                    }
            value = Array (Vector.fromList [Number 1, Number 2])
        assert (not (validateValue schema value))

prop_validate_string_length_violation :: Property
prop_validate_string_length_violation =
    property $ do
        let schema =
                emptySchema
                    { schemaType = Just SString
                    , schemaMinLength = Just 2
                    }
            value = String "a"
        assert (not (validateValue schema value))
