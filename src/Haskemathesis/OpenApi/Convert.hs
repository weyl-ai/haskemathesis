-- | Convert OpenAPI schemas into the internal schema representation.
module Haskemathesis.OpenApi.Convert (
    convertSchema,
) where

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (Scientific, toRealFloat)

import Data.OpenApi (
    AdditionalProperties (..),
    OpenApiItems (..),
    OpenApiType (..),
    Reference (..),
    Referenced (..),
    Schema (..),
 )

import qualified Haskemathesis.Schema as HS

convertSchema :: (Reference -> Maybe Schema) -> Schema -> HS.Schema
convertSchema resolveRef schema =
    HS.emptySchema
        { HS.schemaType = convertType <$> _schemaType schema
        , HS.schemaEnum = _schemaEnum schema
        , HS.schemaConst = Nothing
        , HS.schemaMinLength = fmap fromInteger (_schemaMinLength schema)
        , HS.schemaMaxLength = fmap fromInteger (_schemaMaxLength schema)
        , HS.schemaMinimum = fmap toDouble (_schemaMinimum schema)
        , HS.schemaMaximum = fmap toDouble (_schemaMaximum schema)
        , HS.schemaExclusiveMinimum = exclusiveMin (_schemaExclusiveMinimum schema) (_schemaMinimum schema)
        , HS.schemaExclusiveMaximum = exclusiveMax (_schemaExclusiveMaximum schema) (_schemaMaximum schema)
        , HS.schemaPattern = _schemaPattern schema
        , HS.schemaItems = resolveItems (_schemaItems schema)
        , HS.schemaMinItems = fmap fromInteger (_schemaMinItems schema)
        , HS.schemaMaxItems = fmap fromInteger (_schemaMaxItems schema)
        , HS.schemaUniqueItems = _schemaUniqueItems schema == Just True
        , HS.schemaRequired = _schemaRequired schema
        , HS.schemaProperties = convertProperties (_schemaProperties schema)
        , HS.schemaAdditionalProperties = convertAdditional (_schemaAdditionalProperties schema)
        , HS.schemaAllOf = convertComposed (fromMaybe [] (_schemaAllOf schema))
        , HS.schemaAnyOf = convertComposed (fromMaybe [] (_schemaAnyOf schema))
        , HS.schemaOneOf = convertComposed (fromMaybe [] (_schemaOneOf schema))
        , HS.schemaNullable = _schemaNullable schema == Just True
        }
  where
    resolveItems mitems =
        mitems >>= resolveItemsRef

    resolveItemsRef items =
        case items of
            OpenApiItemsObject ref -> resolveReferencedSchema ref
            OpenApiItemsArray refs -> listToMaybe refs >>= resolveReferencedSchema

    resolveReferencedSchema ref =
        case ref of
            Inline s -> Just (convertSchema resolveRef s)
            Ref r -> fmap (convertSchema resolveRef) (resolveRef r)

    convertProperties props =
        Map.fromList
            [ (name, schema')
            | (name, ref) <- InsOrdHashMap.toList props
            , Just schema' <- [resolveReferencedSchema ref]
            ]

    convertComposed xs =
        [ schema'
        | ref <- xs
        , Just schema' <- [resolveReferencedSchema ref]
        ]

    convertAdditional addl =
        case addl of
            Nothing -> Nothing
            Just (AdditionalPropertiesAllowed True) -> Just HS.AdditionalPropertiesAny
            Just (AdditionalPropertiesAllowed False) -> Just HS.AdditionalPropertiesNone
            Just (AdditionalPropertiesSchema ref) ->
                case resolveReferencedSchema ref of
                    Just schema' -> Just (HS.AdditionalPropertiesSchema schema')
                    Nothing -> Just HS.AdditionalPropertiesNone

    toDouble :: Scientific -> Double
    toDouble = toRealFloat

    exclusiveMin flag mMin =
        case flag of
            Just True -> fmap toDouble mMin
            Just False -> Nothing
            Nothing -> Nothing

    exclusiveMax flag mMax =
        case flag of
            Just True -> fmap toDouble mMax
            Just False -> Nothing
            Nothing -> Nothing

convertType :: OpenApiType -> HS.SchemaType
convertType t =
    case t of
        OpenApiString -> HS.SString
        OpenApiInteger -> HS.SInteger
        OpenApiNumber -> HS.SNumber
        OpenApiBoolean -> HS.SBoolean
        OpenApiArray -> HS.SArray
        OpenApiObject -> HS.SObject
        OpenApiNull -> HS.SNull
