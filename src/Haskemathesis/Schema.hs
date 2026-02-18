{-# LANGUAGE StrictData #-}

{- | Internal JSON Schema representation.

This module provides a simplified JSON Schema type that captures
the subset of JSON Schema features used by Haskemathesis. The 'Schema'
type is used throughout the codebase for generating values and
validating responses.

=== Schema Types

The 'SchemaType' enum represents the basic JSON types:

* 'SString' - Text strings
* 'SInteger' - Whole numbers
* 'SNumber' - Floating point numbers
* 'SBoolean' - True/false values
* 'SArray' - Ordered lists
* 'SObject' - Key-value mappings
* 'SNull' - Null values

=== Creating Schemas

Use 'emptySchema' as a starting point and override fields:

@
import Haskemathesis.Schema

-- A string schema with length constraints
stringSchema :: Schema
stringSchema = emptySchema
    { schemaType = Just SString
    , schemaMinLength = Just 1
    , schemaMaxLength = Just 100
    }

-- An object schema with required fields
userSchema :: Schema
userSchema = emptySchema
    { schemaType = Just SObject
    , schemaRequired = ["name", "email"]
    , schemaProperties = Map.fromList
        [ ("name", emptySchema { schemaType = Just SString })
        , ("email", emptySchema { schemaType = Just SString })
        ]
    }
@
-}
module Haskemathesis.Schema (
    -- * Schema Types
    Schema (..),
    SchemaType (..),
    AdditionalProperties (..),

    -- * Schema Construction
    emptySchema,
)
where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)

{- | Basic JSON Schema types.

These correspond to the JSON Schema type primitive types
and are used to specify what kind of values a schema accepts.
-}
data SchemaType
    = -- | Text strings with optional length and pattern constraints.
      SString
    | -- | Whole numbers (no fractional part).
      SInteger
    | -- | Any numeric value (integer or floating point).
      SNumber
    | -- | True or false values.
      SBoolean
    | -- | Ordered list of values with optional item schema.
      SArray
    | -- | Key-value mapping with property schemas.
      SObject
    | -- | The null value.
      SNull
    deriving (Eq, Show)

{- | Policy for handling additional properties in objects.

JSON Schema allows controlling whether objects can have properties
beyond those explicitly defined in 'schemaProperties'.
-}
data AdditionalProperties
    = -- | Any additional properties are allowed (no validation).
      AdditionalPropertiesAny
    | -- | No additional properties are allowed; extra properties cause validation errors.
      AdditionalPropertiesNone
    | -- | Additional properties must conform to the given schema.
      AdditionalPropertiesSchema Schema
    deriving (Eq, Show)

{- | JSON Schema representation.

This type captures the subset of JSON Schema features used by
Haskemathesis for generating test data and validating responses.

=== Field Descriptions

* 'schemaType' - The JSON type (string, number, etc.)
* 'schemaEnum' - Allowed values (if restricted to specific values)
* 'schemaConst' - Fixed value (if the value must be exactly this)
* 'schemaMinLength' / 'schemaMaxLength' - String length constraints
* 'schemaMinimum' / 'schemaMaximum' - Numeric range constraints
* 'schemaExclusiveMinimum' / 'schemaExclusiveMaximum' - Exclusive bounds
* 'schemaPattern' - Regular expression pattern for strings
* 'schemaItems' - Schema for array items
* 'schemaMinItems' / 'schemaMaxItems' - Array length constraints
* 'schemaUniqueItems' - Whether array items must be unique
* 'schemaRequired' - Required properties for objects
* 'schemaProperties' - Property schemas for objects
* 'schemaAdditionalProperties' - Policy for extra properties
* 'schemaAllOf' / 'schemaAnyOf' / 'schemaOneOf' - Schema combinators
* 'schemaNullable' - Whether null is an acceptable value
-}
data Schema = Schema
    { schemaType :: Maybe SchemaType
    {- ^ The basic JSON type (string, number, object, etc.).
    'Nothing' means any type is acceptable.
    -}
    , schemaEnum :: Maybe [Value]
    -- ^ If present, the value must be one of these specific values.
    , schemaConst :: Maybe Value
    -- ^ If present, the value must be exactly this value.
    , schemaMinLength :: Maybe Int
    -- ^ Minimum length for string values (in bytes).
    , schemaMaxLength :: Maybe Int
    -- ^ Maximum length for string values (in bytes).
    , schemaMinimum :: Maybe Double
    -- ^ Minimum value for numeric types (inclusive).
    , schemaMaximum :: Maybe Double
    -- ^ Maximum value for numeric types (inclusive).
    , schemaExclusiveMinimum :: Maybe Double
    -- ^ Minimum value for numeric types (exclusive).
    , schemaExclusiveMaximum :: Maybe Double
    -- ^ Maximum value for numeric types (exclusive).
    , schemaPattern :: Maybe Text
    -- ^ Regular expression pattern that string values must match.
    , schemaItems :: Maybe Schema
    -- ^ Schema for items in an array.
    , schemaMinItems :: Maybe Int
    -- ^ Minimum number of items in an array.
    , schemaMaxItems :: Maybe Int
    -- ^ Maximum number of items in an array.
    , schemaUniqueItems :: Bool
    -- ^ Whether array items must be unique (default: False).
    , schemaRequired :: [Text]
    -- ^ List of required property names for objects.
    , schemaProperties :: Map Text Schema
    -- ^ Map from property name to schema for object properties.
    , schemaAdditionalProperties :: Maybe AdditionalProperties
    -- ^ Policy for handling properties not in 'schemaProperties'.
    , schemaAllOf :: [Schema]
    -- ^ Schema must satisfy all of these schemas (AND combinator).
    , schemaAnyOf :: [Schema]
    -- ^ Schema must satisfy at least one of these schemas (OR combinator).
    , schemaOneOf :: [Schema]
    -- ^ Schema must satisfy exactly one of these schemas (XOR combinator).
    , schemaNullable :: Bool
    -- ^ Whether null is an acceptable value in addition to the main type.
    }
    deriving (Eq, Show)

{- | An empty schema that accepts any value.

This is the starting point for building schemas. All fields are
set to their most permissive values (usually 'Nothing' or empty lists).

=== Example

@
-- Start with emptySchema and override specific fields
mySchema :: Schema
mySchema = emptySchema
    { schemaType = Just SString
    , schemaMinLength = Just 1
    , schemaMaxLength = Just 100
    }
@
-}
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
