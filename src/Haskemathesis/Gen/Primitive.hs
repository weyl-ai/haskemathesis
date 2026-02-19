{- | Primitive generators for schema leaf types.

This module provides generators for primitive JSON Schema types
(string, integer, number, boolean). These generators respect the
constraints defined in the schema such as min/max length, patterns,
and numeric ranges.

=== Basic Usage

@
import Haskemathesis.Gen.Primitive (genString, genInteger)
import Haskemathesis.Schema (emptySchema, schemaType, SString, SInteger)

-- Generate a string
let stringSchema = emptySchema { schemaType = Just SString }
stringValue <- genString stringSchema

-- Generate an integer
let intSchema = emptySchema { schemaType = Just SInteger }
intValue <- genInteger intSchema
@
-}
module Haskemathesis.Gen.Primitive (
    genString,
    genInteger,
    genNumber,
    genBoolean,
    genConstOrEnum,
)
where

import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import Data.Scientific (fromFloatDigits)
import Haskemathesis.Gen.Pattern (genFromPatternWithLength)
import Haskemathesis.Schema
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Try to generate from const or enum first, otherwise use the fallback generator.
genConstOrEnum :: Schema -> Gen Value -> Gen Value
genConstOrEnum schema fallback =
    case schemaConst schema of
        Just v -> pure v
        Nothing ->
            case schemaEnum schema of
                Just xs | not (null xs) -> Gen.element xs
                _noEnum -> fallback

{- | Generate a string value according to schema constraints.

This generator respects the following schema constraints:

* 'schemaEnum' - If present, selects from the allowed values
* 'schemaConst' - If present, returns the constant value
* 'schemaPattern' - If present, generates strings matching the regex
* 'schemaMinLength' / 'schemaMaxLength' - Controls string length

=== Parameters

* @schema@ - The 'Schema' defining string constraints

=== Return Value

Returns a 'Gen Value' that generates 'String' values.

=== Example

@
let schema = emptySchema
        { schemaType = Just SString
        , schemaMinLength = Just 5
        , schemaMaxLength = Just 10
        }
value <- genString schema
@
-}
genString :: Schema -> Gen Value
genString schema =
    genConstOrEnum schema $
        case schemaPattern schema of
            Just pat ->
                let minL = schemaMinLength schema
                    maxL = schemaMaxLength schema
                 in case genFromPatternWithLength pat minL maxL of
                        Just patGen ->
                            -- Generate from pattern with length constraints
                            String <$> patGen
                        Nothing ->
                            -- Pattern parsing failed or no matches fit length constraints
                            -- Fall back to random alphanumeric string
                            let minLen = fromMaybe 0 minL
                                maxLen = fromMaybe (minLen + 8) maxL
                             in String <$> Gen.text (Range.linear minLen maxLen) Gen.alphaNum
            Nothing ->
                let minL = fromMaybe 0 (schemaMinLength schema)
                    maxL = fromMaybe (minL + 32) (schemaMaxLength schema)
                 in String <$> Gen.text (Range.linear minL maxL) Gen.alphaNum

{- | Generate an integer value according to schema constraints.

This generator respects the following schema constraints:

* 'schemaMinimum' / 'schemaMaximum' - Numeric range bounds
* 'schemaExclusiveMinimum' / 'schemaExclusiveMaximum' - Exclusive bounds

=== Parameters

* @schema@ - The 'Schema' defining integer constraints

=== Return Value

Returns a 'Gen Value' that generates 'Number' values with integer values.

=== Example

@
let schema = emptySchema
        { schemaType = Just SInteger
        , schemaMinimum = Just 0
        , schemaMaximum = Just 100
        }
value <- genInteger schema
@
-}
genInteger :: Schema -> Gen Value
genInteger schema =
    Number . fromIntegral <$> Gen.int (Range.linearFrom 0 lo hi)
  where
    lo :: Int
    lo =
        max
            (maybe (-1000) ceiling (schemaMinimum schema))
            (maybe (-1000) ((+ 1) . floor) (schemaExclusiveMinimum schema))
    hi :: Int
    hi =
        min
            (maybe 1000 floor (schemaMaximum schema))
            (maybe 1000 (subtract 1 . ceiling) (schemaExclusiveMaximum schema))

{- | Generate a floating-point number value according to schema constraints.

This generator respects the following schema constraints:

* 'schemaMinimum' / 'schemaMaximum' - Numeric range bounds
* 'schemaExclusiveMinimum' / 'schemaExclusiveMaximum' - Exclusive bounds

=== Parameters

* @schema@ - The 'Schema' defining number constraints

=== Return Value

Returns a 'Gen Value' that generates 'Number' values with floating-point values.

=== Example

@
let schema = emptySchema
        { schemaType = Just SNumber
        , schemaMinimum = Just 0.0
        , schemaMaximum = Just 1.0
        }
value <- genNumber schema
@
-}
genNumber :: Schema -> Gen Value
genNumber schema =
    Number . fromFloatDigits
        <$> Gen.double (Range.linearFracFrom 0 lo hi)
  where
    lo =
        case schemaExclusiveMinimum schema of
            Just minV -> minV + 1e-6
            Nothing -> fromMaybe (-1000) (schemaMinimum schema)
    hi =
        case schemaExclusiveMaximum schema of
            Just maxV -> maxV - 1e-6
            Nothing -> fromMaybe 1000 (schemaMaximum schema)

{- | Generate a boolean value.

Generates either 'True' or 'False' with equal probability.

=== Parameters

* @schema@ - The 'Schema' (unused, but kept for API consistency)

=== Return Value

Returns a 'Gen Value' that generates 'Bool' values.

=== Example

@
let schema = emptySchema { schemaType = Just SBoolean }
value <- genBoolean schema
@
-}
genBoolean :: Schema -> Gen Value
genBoolean _ = Bool <$> Gen.bool
