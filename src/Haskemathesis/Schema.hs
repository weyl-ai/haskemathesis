{-# LANGUAGE StrictData #-}

module Haskemathesis.Schema (
    Schema (..),
    SchemaType (..),
    AdditionalProperties (..),
    emptySchema,
) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

data SchemaType
    = SString
    | SInteger
    | SNumber
    | SBoolean
    | SArray
    | SObject
    | SNull
    deriving (Eq, Show)

data AdditionalProperties
    = AdditionalPropertiesAny
    | AdditionalPropertiesNone
    | AdditionalPropertiesSchema Schema
    deriving (Eq, Show)

data Schema = Schema
    { schemaType :: Maybe SchemaType
    , schemaEnum :: Maybe [Value]
    , schemaConst :: Maybe Value
    , schemaMinLength :: Maybe Int
    , schemaMaxLength :: Maybe Int
    , schemaMinimum :: Maybe Double
    , schemaMaximum :: Maybe Double
    , schemaExclusiveMinimum :: Maybe Double
    , schemaExclusiveMaximum :: Maybe Double
    , schemaPattern :: Maybe Text
    , schemaItems :: Maybe Schema
    , schemaMinItems :: Maybe Int
    , schemaMaxItems :: Maybe Int
    , schemaUniqueItems :: Bool
    , schemaRequired :: [Text]
    , schemaProperties :: Map Text Schema
    , schemaAdditionalProperties :: Maybe AdditionalProperties
    , schemaAllOf :: [Schema]
    , schemaAnyOf :: [Schema]
    , schemaOneOf :: [Schema]
    , schemaNullable :: Bool
    }
    deriving (Eq, Show)

emptySchema :: Schema
emptySchema =
    Schema
        { schemaType = Nothing
        , schemaEnum = Nothing
        , schemaConst = Nothing
        , schemaMinLength = Nothing
        , schemaMaxLength = Nothing
        , schemaMinimum = Nothing
        , schemaMaximum = Nothing
        , schemaExclusiveMinimum = Nothing
        , schemaExclusiveMaximum = Nothing
        , schemaPattern = Nothing
        , schemaItems = Nothing
        , schemaMinItems = Nothing
        , schemaMaxItems = Nothing
        , schemaUniqueItems = False
        , schemaRequired = []
        , schemaProperties = mempty
        , schemaAdditionalProperties = Nothing
        , schemaAllOf = []
        , schemaAnyOf = []
        , schemaOneOf = []
        , schemaNullable = False
        }
