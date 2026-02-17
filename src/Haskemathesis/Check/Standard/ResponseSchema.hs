{-# LANGUAGE OverloadedStrings #-}

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
