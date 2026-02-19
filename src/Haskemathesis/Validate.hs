{-# LANGUAGE OverloadedStrings #-}

{- | Validation utilities for JSON values against schemas.

This module provides functions for validating JSON 'Value's against
'Schema' definitions. It's used internally by generators to ensure
generated values conform to schemas, and by checks to validate
API responses.

=== Basic Validation

Check if a value satisfies a schema:

@
import Haskemathesis.Validate (validateValue)
import Haskemathesis.Schema (emptySchema, schemaType, SString)
import Data.Aeson (String)

schema :: Schema
schema = emptySchema { schemaType = Just SString }

-- Returns True
isValid = validateValue schema (String "hello")
@

=== Getting Detailed Errors

For debugging or reporting, get specific validation errors:

@
import Haskemathesis.Validate (validateErrors)

errors = validateErrors schema value
-- Returns a list of error messages like:
-- ["string shorter than minLength", "number below minimum"]
@

=== Supported Validations

The validator supports all common JSON Schema constraints:

* Type validation ('SString', 'SInteger', 'SNumber', 'SBoolean', 'SArray', 'SObject', 'SNull')
* Enum and const constraints
* String: min/max length, pattern matching
* Numbers: minimum, maximum, exclusive bounds
* Arrays: min/max items, unique items, item schema validation
* Objects: required properties, property schemas, additional properties
* Combinators: allOf, anyOf, oneOf
* Nullable types
-}
module Haskemathesis.Validate (
    -- * Validation Functions
    validateValue,
    validateErrors,
)
where

import Data.Aeson (Value (..))
import Data.Aeson.Key (Key, fromText, toText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Scientific (isInteger, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Haskemathesis.Schema
import Text.Regex.TDFA ((=~))

{- | Validate a JSON value against a schema.

Returns 'True' if the value conforms to the schema, 'False' otherwise.
This is a convenience function equivalent to checking if 'validateErrors'
returns an empty list.

=== Example

@
schema :: Schema
schema = emptySchema { schemaType = Just SString }

isValid = validateValue schema (String "hello")  -- True
isInvalid = validateValue schema (Number 42)     -- False
@
-}
validateValue :: Schema -> Value -> Bool
validateValue schema value = null (validateErrors schema value)

{- | Validate a JSON value and return detailed error messages.

Returns a list of error messages describing why the value does not
conform to the schema. If the list is empty, the value is valid.

=== Example

@
schema :: Schema
schema = emptySchema
    { schemaType = Just SString
    , schemaMinLength = Just 5
    }

errors = validateErrors schema (String "hi")
-- Returns: ["string shorter than minLength"]
@
-}
validateErrors :: Schema -> Value -> [Text]
validateErrors schema value =
    concat
        [ validateType schema value
        , validateEnum schema value
        , validateConst schema value
        , validateString schema value
        , validateNumber schema value
        , validateArray schema value
        , validateObject schema value
        , validateCombinators schema value
        ]

-- | Validate the type of a value.
validateType :: Schema -> Value -> [Text]
validateType schema value
    | value == Null && schemaNullable schema = []
    | otherwise =
        case schemaType schema of
            Nothing -> []
            Just st -> checkType st value

-- | Check if a value matches the expected schema type.
checkType :: SchemaType -> Value -> [Text]
checkType SString (String _) = []
checkType SString _ = ["expected string"]
checkType SInteger (Number n) | isInteger n = []
checkType SInteger _ = ["expected integer"]
checkType SNumber (Number _) = []
checkType SNumber _ = ["expected number"]
checkType SBoolean (Bool _) = []
checkType SBoolean _ = ["expected boolean"]
checkType SArray (Array _) = []
checkType SArray _ = ["expected array"]
checkType SObject (Object _) = []
checkType SObject _ = ["expected object"]
checkType SNull Null = []
checkType SNull _ = ["expected null"]

-- | Validate enum constraints.
validateEnum :: Schema -> Value -> [Text]
validateEnum schema value =
    case schemaEnum schema of
        Nothing -> []
        Just xs ->
            ["value not in enum" | value `notElem` xs]

-- | Validate const constraints.
validateConst :: Schema -> Value -> [Text]
validateConst schema value =
    case schemaConst schema of
        Nothing -> []
        Just v ->
            ["value not equal to const" | value /= v]

-- | Validate string constraints (length, pattern).
validateString :: Schema -> Value -> [Text]
validateString schema value =
    case value of
        String txt ->
            concat
                [ case schemaMinLength schema of
                    Nothing -> []
                    Just minL ->
                        ["string shorter than minLength" | textLength txt < minL]
                , case schemaMaxLength schema of
                    Nothing -> []
                    Just maxL ->
                        ["string longer than maxLength" | textLength txt > maxL]
                , case schemaPattern schema of
                    Nothing -> []
                    Just pat ->
                        ["string does not match pattern" | not (T.unpack txt =~ T.unpack pat)]
                ]
        _otherValue -> []

-- | Validate number constraints (range, bounds).
validateNumber :: Schema -> Value -> [Text]
validateNumber schema value =
    case value of
        Number n ->
            let d = toRealFloat n :: Double
             in concat
                    [ case schemaMinimum schema of
                        Nothing -> []
                        Just minV ->
                            ["number below minimum" | d < minV]
                    , case schemaMaximum schema of
                        Nothing -> []
                        Just maxV ->
                            ["number above maximum" | d > maxV]
                    , case schemaExclusiveMinimum schema of
                        Nothing -> []
                        Just minV ->
                            ["number below or equal exclusiveMinimum" | d <= minV]
                    , case schemaExclusiveMaximum schema of
                        Nothing -> []
                        Just maxV ->
                            ["number above or equal exclusiveMaximum" | d >= maxV]
                    ]
        _otherValue -> []

-- | Validate array constraints (length, uniqueness, item schema).
validateArray :: Schema -> Value -> [Text]
validateArray schema value =
    case value of
        Array vec ->
            let items = Vector.toList vec
                len = Vector.length vec
             in concat
                    [ case schemaMinItems schema of
                        Nothing -> []
                        Just minI ->
                            ["array shorter than minItems" | len < minI]
                    , case schemaMaxItems schema of
                        Nothing -> []
                        Just maxI ->
                            ["array longer than maxItems" | len > maxI]
                    , ["array has duplicate items" | schemaUniqueItems schema && hasDuplicateValues vec]
                    , case schemaItems schema of
                        Nothing -> []
                        Just itemSchema ->
                            concatMap (validateErrors itemSchema) items
                    ]
        _otherValue -> []

-- | Validate object constraints (required properties, property schemas, additional properties).
validateObject :: Schema -> Value -> [Text]
validateObject schema value =
    case value of
        Object obj ->
            concat
                [ validateRequired obj
                , validateProperties obj
                , validateAdditional obj
                ]
        _otherValue -> []
  where
    validateRequired obj =
        [ "missing required property: " <> req
        | req <- schemaRequired schema
        , isNothing (KeyMap.lookup (toKey req) obj)
        ]

    validateProperties obj =
        concat
            [ case KeyMap.lookup (toKey name) obj of
                Nothing -> []
                Just v -> validateErrors propSchema v
            | (name, propSchema) <- Map.toList (schemaProperties schema)
            ]

    validateAdditional obj =
        case schemaAdditionalProperties schema of
            Nothing -> []
            Just AdditionalPropertiesAny -> []
            Just AdditionalPropertiesNone ->
                let allowed = Map.keysSet (schemaProperties schema)
                    extras =
                        [ toText k
                        | k <- KeyMap.keys obj
                        , not (toText k `Set.member` allowed)
                        ]
                 in ["additional properties not allowed" | not (null extras)]
            Just (AdditionalPropertiesSchema extraSchema) ->
                let allowed = Map.keysSet (schemaProperties schema)
                    extras =
                        [ v
                        | (k, v) <- KeyMap.toList obj
                        , not (toText k `Set.member` allowed)
                        ]
                 in concatMap (validateErrors extraSchema) extras

-- | Validate schema combinators (allOf, anyOf, oneOf).
validateCombinators :: Schema -> Value -> [Text]
validateCombinators schema value =
    concat
        [ if null (schemaAllOf schema)
            then []
            else concatMap (`validateErrors` value) (schemaAllOf schema)
        , [ "value does not satisfy anyOf"
          | not
                ( null (schemaAnyOf schema)
                    || any (null . (`validateErrors` value)) (schemaAnyOf schema)
                )
          ]
        , if null (schemaOneOf schema)
            then []
            else
                let matches = countMatches value (schemaOneOf schema)
                 in ["value does not satisfy oneOf" | matches /= 1]
        ]

-- | Convert Text to a Key for object property lookup.
toKey :: Text -> Key
toKey = fromText

-- | Calculate the byte length of a Text value.
textLength :: Text -> Int
textLength = BS.length . encodeUtf8

-- | Check if a vector contains duplicate values.
hasDuplicateValues :: Vector.Vector Value -> Bool
hasDuplicateValues = snd . Vector.foldl' step (Set.empty, False)
  where
    step (seen, True) _item = (seen, True)
    step (seen, False) item
        | Set.member item seen = (seen, True)
        | otherwise = (Set.insert item seen, False)

-- | Count how many schemas in a list validate successfully against a value.
countMatches :: Value -> [Schema] -> Int
countMatches value = foldl' step 0
  where
    step acc schema
        | null (validateErrors schema value) = acc + 1
        | otherwise = acc
