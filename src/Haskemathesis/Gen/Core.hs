module Haskemathesis.Gen.Core (
    genFromSchema,
    genFromSchemaWithDepth,
) where

import Data.Aeson (Value (..))
import Data.Aeson.Key (Key, fromText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Haskemathesis.Gen.Primitive
import Haskemathesis.Schema
import Haskemathesis.Validate (validateValue)

genFromSchema :: Schema -> Gen Value
genFromSchema = genFromSchemaWithDepth 4

genFromSchemaWithDepth :: Int -> Schema -> Gen Value
genFromSchemaWithDepth depth schema
    | depth <= 0 = genLeafFallback schema
    | otherwise =
        if schemaNullable schema
            then Gen.choice [pure Null, genNonNull]
            else genNonNull
  where
    genNonNull =
        genWithCombinators depth schema

genWithCombinators :: Int -> Schema -> Gen Value
genWithCombinators depth schema
    | not (null (schemaOneOf schema)) =
        Gen.filter (validateValue schema) $
            Gen.choice (map (genFromSchemaWithDepth (depth - 1)) (schemaOneOf schema))
    | not (null (schemaAnyOf schema)) =
        Gen.filter (validateValue schema) $
            Gen.choice (map (genFromSchemaWithDepth (depth - 1)) (schemaAnyOf schema))
    | not (null (schemaAllOf schema)) =
        Gen.filter (validateValue schema) $
            genFromConstraints depth schema
    | otherwise = genBase depth schema

genBase :: Int -> Schema -> Gen Value
genBase depth schema =
    case schemaConst schema of
        Just v -> pure v
        Nothing ->
            case schemaEnum schema of
                Just xs | not (null xs) -> Gen.element xs
                _ ->
                    case schemaType schema of
                        Just SString -> genString schema
                        Just SInteger -> genInteger schema
                        Just SNumber -> genNumber schema
                        Just SBoolean -> genBoolean schema
                        Just SArray -> genArray depth schema
                        Just SObject -> genObject depth schema
                        Just SNull -> pure Null
                        Nothing -> genFromConstraints depth schema

genFromConstraints :: Int -> Schema -> Gen Value
genFromConstraints depth schema =
    Gen.choice
        [ genString schema
        , genInteger schema
        , genNumber schema
        , genBoolean schema
        , genArray depth schema
        , genObject depth schema
        , pure Null
        ]

genArray :: Int -> Schema -> Gen Value
genArray depth schema = do
    let minI = fromMaybe 0 (schemaMinItems schema)
        maxI = fromMaybe (minI + 5) (schemaMaxItems schema)
        itemSchema = fromMaybe emptySchema (schemaItems schema)
    let itemGen = genFromSchemaWithDepth (depth - 1) itemSchema
        listGen = Gen.list (Range.linear minI maxI) itemGen
    items <-
        if schemaUniqueItems schema
            then Gen.filter (\xs -> length (nub xs) == length xs) listGen
            else listGen
    pure (Array (Vector.fromList items))

genObject :: Int -> Schema -> Gen Value
genObject depth schema = do
    let props = Map.toList (schemaProperties schema)
        required = schemaRequired schema
    reqFields <-
        traverse
            ( \(k, v) -> do
                val <- genFromSchemaWithDepth (depth - 1) v
                pure (k, val)
            )
            [ (k, v)
            | (k, v) <- props
            , k `elem` required
            ]
    optFields <-
        traverse
            ( \(k, v) -> do
                mv <- Gen.maybe (genFromSchemaWithDepth (depth - 1) v)
                pure (k, mv)
            )
            [ (k, v)
            | (k, v) <- props
            , k `notElem` required
            ]
    extras <- genAdditional depth schema
    let pairs =
            [(fromText k, val) | (k, val) <- reqFields]
                ++ [(fromText k, val) | (k, Just val) <- optFields]
                ++ extras
    pure (Object (KeyMap.fromList pairs))

genAdditional :: Int -> Schema -> Gen [(Key, Value)]
genAdditional depth schema =
    case schemaAdditionalProperties schema of
        Nothing -> pure []
        Just AdditionalPropertiesNone -> pure []
        Just AdditionalPropertiesAny ->
            Gen.list (Range.linear 0 2) $ do
                key <- Gen.text (Range.linear 1 8) Gen.alphaNum
                val <- genFromSchemaWithDepth (depth - 1) emptySchema
                pure (fromText key, val)
        Just (AdditionalPropertiesSchema extraSchema) ->
            Gen.list (Range.linear 0 2) $ do
                key <- Gen.text (Range.linear 1 8) Gen.alphaNum
                val <- genFromSchemaWithDepth (depth - 1) extraSchema
                pure (fromText key, val)

genLeafFallback :: Schema -> Gen Value
genLeafFallback schema =
    case schemaType schema of
        Just SString -> genString schema
        Just SInteger -> genInteger schema
        Just SNumber -> genNumber schema
        Just SBoolean -> genBoolean schema
        Just SArray -> genArray 0 schema
        Just SObject -> genObject 0 schema
        Just SNull -> pure Null
        Nothing -> genFromConstraints 0 schema
