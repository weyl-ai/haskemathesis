{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Render (spec) where

import qualified Data.Text as T
import Hedgehog (Property, assert, property)
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Types (FailureDetail (..))
import Haskemathesis.Execute.Types (ApiRequest (..), ApiResponse (..))
import Haskemathesis.Report.Render (renderFailureDetail, renderFailureDetailAnsi)
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Render" $ do
        itProp "includes schema errors" prop_render_failure_includes_schema_errors
        itProp "includes schema diff" prop_render_failure_includes_schema_diff
        itProp "includes mutation" prop_render_failure_includes_mutation
        itProp "ansi includes color codes" prop_render_failure_includes_ansi
        itProp "ansi includes seed" prop_render_failure_includes_seed_ansi
        itProp "includes seed" prop_render_failure_includes_seed
        itProp "includes request and response" prop_render_failure_includes_request_and_response

prop_render_failure_includes_schema_errors :: Property
prop_render_failure_includes_schema_errors =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = ["err-1"]
                    , fdSchemaDiff = Nothing
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Schema errors: err-1" `T.isInfixOf` rendered)

prop_render_failure_includes_schema_diff :: Property
prop_render_failure_includes_schema_diff =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Just "diff line"
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Schema diff:" `T.isInfixOf` rendered)
        assert ("diff line" `T.isInfixOf` rendered)

prop_render_failure_includes_mutation :: Property
prop_render_failure_includes_mutation =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Nothing
                    , fdMutation = Just "invalid body"
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Mutation: invalid body" `T.isInfixOf` rendered)

prop_render_failure_includes_seed :: Property
prop_render_failure_includes_seed =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Nothing
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetail Nothing (Just "seed-123") detail
        assert ("Seed: seed-123" `T.isInfixOf` rendered)

prop_render_failure_includes_ansi :: Property
prop_render_failure_includes_ansi =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = ["err-1"]
                    , fdSchemaDiff = Nothing
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetailAnsi Nothing Nothing detail
        assert ("\ESC[" `T.isInfixOf` rendered)
        assert ("Schema errors: err-1" `T.isInfixOf` rendered)

prop_render_failure_includes_seed_ansi :: Property
prop_render_failure_includes_seed_ansi =
    property $ do
        let detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = dummyRequest emptyOperation
                    , fdResponse =
                        ApiResponse
                            { resStatusCode = 200
                            , resHeaders = []
                            , resBody = ""
                            , resTime = 0
                            }
                    , fdOperation = "GET /"
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Nothing
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetailAnsi Nothing (Just "seed-xyz") detail
        assert ("Seed: seed-xyz" `T.isInfixOf` rendered)

prop_render_failure_includes_request_and_response :: Property
prop_render_failure_includes_request_and_response =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = [("q", "a")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            res =
                ApiResponse
                    { resStatusCode = 500
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
            detail =
                FailureDetail
                    { fdCheck = "check"
                    , fdMessage = "message"
                    , fdRequest = req
                    , fdResponse = res
                    , fdOperation = "GET /items"
                    , fdSchemaErrors = []
                    , fdSchemaDiff = Nothing
                    , fdMutation = Nothing
                    }
            rendered = renderFailureDetail Nothing Nothing detail
        assert ("Request: GET /items?q=a" `T.isInfixOf` rendered)
        assert ("Response: status=500" `T.isInfixOf` rendered)
