{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.StatusCode (spec) where

import qualified Data.Map.Strict as Map
import Hedgehog (Property, failure, property, success)
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Standard (statusCodeConformance)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResponseSpec (..))
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Status code conformance" $ do
        itProp "default allows" prop_status_code_conformance_default_allows
        itProp "missing fails" prop_status_code_conformance_missing_fails
        itProp "explicit allows" prop_status_code_conformance_explicit_allows

prop_status_code_conformance_default_allows :: Property
prop_status_code_conformance_default_allows =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roDefaultResponse = Just responseSpec}
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure

prop_status_code_conformance_missing_fails :: Property
prop_status_code_conformance_missing_fails =
    property $ do
        let op = emptyOperation
            res =
                ApiResponse
                    { resStatusCode = 418
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckFailed _ -> success
            CheckPassed -> failure

prop_status_code_conformance_explicit_allows :: Property
prop_status_code_conformance_explicit_allows =
    property $ do
        let responseSpec =
                ResponseSpec
                    { rsContent = mempty
                    , rsHeaders = mempty
                    , rsRequiredHeaders = []
                    }
            op = emptyOperation{roResponses = Map.fromList [(201, responseSpec)]}
            res =
                ApiResponse
                    { resStatusCode = 201
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case checkRun statusCodeConformance (dummyRequest op) res op of
            CheckPassed -> success
            CheckFailed _ -> failure
