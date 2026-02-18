{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.ResponseSchema (spec) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Haskemathesis.Check.Standard (responseSchemaConformance)
import Haskemathesis.Check.Types (Check (..), CheckResult (..), FailureDetail (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResponseSpec (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)
import Hedgehog (Property, assert, failure, property, success)
import Network.HTTP.Types (hContentType)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
    describe "Response schema conformance" $ do
        itProp "schema errors reported" prop_schema_errors_reported
        itProp "schema diff reported" prop_schema_diff_reported
        itProp "without content-type and single schema" prop_response_schema_without_content_type_single_schema
        itProp "without content-type and multiple schemas" prop_response_schema_without_content_type_multiple_schemas_skips
        itProp "invalid json fails" prop_response_schema_conformance_invalid_json_fails
        itProp "null allowed when nullable" prop_response_schema_conformance_null_allows_nullable
        itProp "empty body fails" prop_response_schema_conformance_empty_body_fails
        itProp "no schema skips" prop_response_schema_conformance_no_schema_skips
        itProp "default response used" prop_response_schema_conformance_default_used
        itProp "integer schema ok" prop_response_schema_conformance_integer_ok
        itProp "number schema ok" prop_response_schema_conformance_number_ok
        -- New response validation tests
        itProp "nested object validated" prop_response_schema_nested_object_validated
        itProp "oneOf selects correct branch" prop_response_schema_oneof_selects_correct
        itProp "anyOf allows multiple" prop_response_schema_anyof_allows_multiple
        itProp "allOf requires all" prop_response_schema_allof_requires_all

prop_schema_errors_reported :: Property
prop_schema_errors_reported =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = LBS.toStrict (encode ("oops" :: Text))
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed detail -> assert (not (null (fdSchemaErrors detail)))
            _otherResult -> failure

prop_schema_diff_reported :: Property
prop_schema_diff_reported =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = LBS.toStrict (encode ("oops" :: Text))
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed detail -> assert (isJust (fdSchemaDiff detail))
            _otherResult -> failure

prop_response_schema_without_content_type_single_schema :: Property
prop_response_schema_without_content_type_single_schema =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_without_content_type_multiple_schemas_skips :: Property
prop_response_schema_without_content_type_multiple_schemas_skips =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent =
                        Map.fromList
                            [ ("application/json", emptySchema{schemaType = Just SString})
                            , ("text/plain", emptySchema{schemaType = Just SString})
                            ]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode ("ok" :: Text))
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_invalid_json_fails :: Property
prop_response_schema_conformance_invalid_json_fails =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "not-json"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_schema_conformance_null_allows_nullable :: Property
prop_response_schema_conformance_null_allows_nullable =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent =
                        Map.fromList
                            [
                                ( "application/json"
                                , emptySchema
                                    { schemaType = Just SString
                                    , schemaNullable = True
                                    }
                                )
                            ]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "null"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_empty_body_fails :: Property
prop_response_schema_conformance_empty_body_fails =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SString})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_response_schema_conformance_no_schema_skips :: Property
prop_response_schema_conformance_no_schema_skips =
    property $ do
        let op = emptyOperation
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "not-json"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_default_used :: Property
prop_response_schema_conformance_default_used =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roDefaultResponse = Just responseSpec}
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_integer_ok :: Property
prop_response_schema_conformance_integer_ok =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SInteger})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_response_schema_conformance_number_ok :: Property
prop_response_schema_conformance_number_ok =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", emptySchema{schemaType = Just SNumber})]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "1.5"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

-- | Nested objects should be validated recursively
prop_response_schema_nested_object_validated :: Property
prop_response_schema_nested_object_validated =
    property $ do
        let innerSchema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("value", emptySchema{schemaType = Just SInteger})]
                    , schemaRequired = ["value"]
                    }
            outerSchema =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("nested", innerSchema)]
                    , schemaRequired = ["nested"]
                    }
            responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", outerSchema)]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            -- Valid nested object
            validRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "{\"nested\":{\"value\":42}}"
                    , resTime = 0
                    }
            -- Invalid: nested.value is string instead of integer
            invalidRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "{\"nested\":{\"value\":\"wrong\"}}"
                    , resTime = 0
                    }
        -- Valid should pass
        case checkRun responseSchemaConformance (dummyRequest op) validRes op of
            CheckPassed -> success
            CheckFailed _ -> failure
        -- Invalid should fail
        case checkRun responseSchemaConformance (dummyRequest op) invalidRes op of
            CheckFailed _ -> success
            CheckPassed -> failure

-- | OneOf should select the correct branch when value matches exactly one
prop_response_schema_oneof_selects_correct :: Property
prop_response_schema_oneof_selects_correct =
    property $ do
        let stringSchema = emptySchema{schemaType = Just SString}
            integerSchema = emptySchema{schemaType = Just SInteger}
            oneOfSchema =
                emptySchema
                    { schemaOneOf = [stringSchema, integerSchema]
                    }
            responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", oneOfSchema)]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            -- String value should match string branch
            stringRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "\"hello\""
                    , resTime = 0
                    }
            -- Integer value should match integer branch
            intRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "42"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) stringRes op of
            CheckPassed -> success
            CheckFailed _ -> failure
        case checkRun responseSchemaConformance (dummyRequest op) intRes op of
            CheckPassed -> success
            CheckFailed _ -> failure

-- | AnyOf should allow values matching any of the schemas
prop_response_schema_anyof_allows_multiple :: Property
prop_response_schema_anyof_allows_multiple =
    property $ do
        let stringSchema = emptySchema{schemaType = Just SString}
            integerSchema = emptySchema{schemaType = Just SInteger}
            anyOfSchema =
                emptySchema
                    { schemaAnyOf = [stringSchema, integerSchema]
                    }
            responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", anyOfSchema)]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            -- String should pass
            stringRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "\"test\""
                    , resTime = 0
                    }
            -- Integer should pass
            intRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "123"
                    , resTime = 0
                    }
            -- Boolean should fail (not in anyOf)
            boolRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "true"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) stringRes op of
            CheckPassed -> success
            CheckFailed _ -> failure
        case checkRun responseSchemaConformance (dummyRequest op) intRes op of
            CheckPassed -> success
            CheckFailed _ -> failure
        case checkRun responseSchemaConformance (dummyRequest op) boolRes op of
            CheckFailed _ -> success
            CheckPassed -> failure

-- | AllOf should require value to match all schemas
prop_response_schema_allof_requires_all :: Property
prop_response_schema_allof_requires_all =
    property $ do
        -- AllOf with object schemas that have different required properties
        let schema1 =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("name", emptySchema{schemaType = Just SString})]
                    , schemaRequired = ["name"]
                    }
            schema2 =
                emptySchema
                    { schemaType = Just SObject
                    , schemaProperties = Map.fromList [("age", emptySchema{schemaType = Just SInteger})]
                    , schemaRequired = ["age"]
                    }
            allOfSchema =
                emptySchema
                    { schemaAllOf = [schema1, schema2]
                    }
            responseSpec =
                ResponseSpec
                    { rsContent = Map.fromList [("application/json", allOfSchema)]
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(200, responseSpec)]}
            -- Valid: has both name and age
            validRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "{\"name\":\"Alice\",\"age\":30}"
                    , resTime = 0
                    }
            -- Invalid: missing age
            missingAgeRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "{\"name\":\"Bob\"}"
                    , resTime = 0
                    }
            -- Invalid: missing name
            missingNameRes =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = [(hContentType, "application/json")]
                    , resBody = "{\"age\":25}"
                    , resTime = 0
                    }
        case checkRun responseSchemaConformance (dummyRequest op) validRes op of
            CheckPassed -> success
            CheckFailed _ -> failure
        case checkRun responseSchemaConformance (dummyRequest op) missingAgeRes op of
            CheckFailed _ -> success
            CheckPassed -> failure
        case checkRun responseSchemaConformance (dummyRequest op) missingNameRes op of
            CheckFailed _ -> success
            CheckPassed -> failure
