{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Validate (
    validateValue,
    validateErrors,
) where

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
import Text.Regex.TDFA ((=~))

import Haskemathesis.Schema

validateValue :: Schema -> Value -> Bool
validateValue schema value = null (validateErrors schema value)

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

validateType :: Schema -> Value -> [Text]
validateType schema value
    | value == Null && schemaNullable schema = []
    | otherwise =
        case schemaType schema of
            Nothing -> []
            Just SString ->
                case value of
                    String _ -> []
                    _otherValue -> ["expected string"]
            Just SInteger ->
                case value of
                    Number n | isInteger n -> []
                    _otherValue -> ["expected integer"]
            Just SNumber ->
                case value of
                    Number _ -> []
                    _otherValue -> ["expected number"]
            Just SBoolean ->
                case value of
                    Bool _ -> []
                    _otherValue -> ["expected boolean"]
            Just SArray ->
                case value of
                    Array _ -> []
                    _otherValue -> ["expected array"]
            Just SObject ->
                case value of
                    Object _ -> []
                    _otherValue -> ["expected object"]
            Just SNull ->
                case value of
                    Null -> []
                    _otherValue -> ["expected null"]

validateEnum :: Schema -> Value -> [Text]
validateEnum schema value =
    case schemaEnum schema of
        Nothing -> []
        Just xs ->
            ["value not in enum" | value `notElem` xs]

validateConst :: Schema -> Value -> [Text]
validateConst schema value =
    case schemaConst schema of
        Nothing -> []
        Just v ->
            ["value not equal to const" | value /= v]

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

toKey :: Text -> Key
toKey = fromText

textLength :: Text -> Int
textLength = BS.length . encodeUtf8

hasDuplicateValues :: Vector.Vector Value -> Bool
hasDuplicateValues = snd . Vector.foldl' step (Set.empty, False)
  where
    step (seen, True) _item = (seen, True)
    step (seen, False) item
        | Set.member item seen = (seen, True)
        | otherwise = (Set.insert item seen, False)

countMatches :: Value -> [Schema] -> Int
countMatches value = foldl' step 0
  where
    step acc schema
        | null (validateErrors schema value) = acc + 1
        | otherwise = acc
