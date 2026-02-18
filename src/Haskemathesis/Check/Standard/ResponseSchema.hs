{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Check.Standard.ResponseSchema
Description : Response body schema validation
Stability   : experimental

This module provides checks for validating API response bodies against
their documented JSON schemas in the OpenAPI specification.
-}
module Haskemathesis.Check.Standard.ResponseSchema (responseSchemaConformance) where

import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Haskemathesis.Check.Standard.Helpers (failureDetail, findResponseSchema)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.Schema (Schema (..), SchemaType (..))
import Haskemathesis.Validate (validateErrors)

{- | Check that the response body conforms to the documented JSON schema.

This check validates the response body against the JSON Schema defined
in the OpenAPI specification for the given status code and content type.
It performs full schema validation including type checks, constraints,
and nested object/array validation.

==== Behavior

* __Passes__ when no schema is defined for the response
* __Passes__ when the response body validates against the schema
* __Fails__ when the response body is not valid JSON
* __Fails__ when the response body violates schema constraints

==== Validation includes

* Type checking (string, integer, number, boolean, array, object, null)
* String constraints (minLength, maxLength, pattern, format)
* Numeric constraints (minimum, maximum, exclusiveMinimum, exclusiveMaximum)
* Array constraints (minItems, maxItems, uniqueItems)
* Object constraints (required properties, additionalProperties)
* Composition keywords (oneOf, anyOf, allOf)

==== Example

@
-- Include schema validation in your checks
checks = ['responseSchemaConformance', 'notAServerError']
@
-}
responseSchemaConformance :: Check
responseSchemaConformance =
    Check "response_schema_conformance" $ \req res op ->
        case findResponseSchema res op of
            Nothing -> CheckPassed
            Just schema ->
                case eitherDecode (LBS.fromStrict (resBody res)) of
                    Left err ->
                        CheckFailed
                            ( failureDetail
                                "response_schema_conformance"
                                ("response body is not valid JSON: " <> T.pack err)
                                []
                                Nothing
                                req
                                res
                                op
                            )
                    Right value ->
                        case validateErrors schema value of
                            [] -> CheckPassed
                            errs ->
                                let diff = schemaDiff schema value errs
                                 in CheckFailed
                                        ( failureDetail
                                            "response_schema_conformance"
                                            ("response violates schema: " <> T.intercalate "; " errs)
                                            errs
                                            (Just diff)
                                            req
                                            res
                                            op
                                        )

schemaDiff :: Schema -> Value -> [Text] -> Text
schemaDiff schema value errs =
    T.intercalate
        "\n"
        ( [ "Expected value to satisfy schema:"
          , "  type: " <> renderSchemaType (schemaType schema)
          ]
            <> map ("  - " <>) errs
            <> ["Actual value: " <> decodeUtf8 (LBS.toStrict (encode value))]
        )

renderSchemaType :: Maybe SchemaType -> Text
renderSchemaType mType =
    case mType of
        Nothing -> "unspecified"
        Just SString -> "string"
        Just SInteger -> "integer"
        Just SNumber -> "number"
        Just SBoolean -> "boolean"
        Just SArray -> "array"
        Just SObject -> "object"
        Just SNull -> "null"
