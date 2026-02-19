{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Negative.Checks (spec) where

import Data.Aeson (Value (..), encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Hedgehog (Property, assert, property, success)
import Test.Hspec (Spec, describe)

import Haskemathesis.Check.Negative (negativeTestRejection)
import Haskemathesis.Check.Types (CheckResult (..), FailureDetail (..))
import Haskemathesis.Config (TestConfig (..), defaultTestConfig)
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.Property (propertiesForSpecWithConfig)
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Negative checks" $ do
        itProp "negative properties appear when enabled" prop_negative_properties_enabled
        itProp "negative properties not present when disabled" prop_negative_properties_disabled
        itProp "negative test rejection passes for 4xx" prop_negative_rejection_passes_4xx
        itProp "negative test rejection expects 4xx" prop_negative_rejection_requires_4xx
        itProp "negative test rejection fails for 2xx" prop_negative_rejection_fails_2xx
        itProp "negative failure includes mutation" prop_negative_failure_includes_mutation

prop_negative_rejection_requires_4xx :: Property
prop_negative_rejection_requires_4xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 500
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "oops"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed _ -> success
            CheckPassed -> assert False

prop_negative_rejection_passes_4xx :: Property
prop_negative_rejection_passes_4xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 400
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "oops"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckPassed -> success
            CheckFailed _ -> assert False

prop_negative_rejection_fails_2xx :: Property
prop_negative_rejection_fails_2xx =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = LBS.toStrict (encode (String "ok"))
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed _ -> success
            CheckPassed -> assert False

prop_negative_failure_includes_mutation :: Property
prop_negative_failure_includes_mutation =
    property $ do
        let op = emptyOperation
            req = dummyRequest op
            res =
                ApiResponse
                    { resStatusCode = 200
                    , resHeaders = []
                    , resBody = ""
                    , resTime = 0
                    }
        case negativeTestRejection "invalid body" req res op of
            CheckFailed detail -> assert (fdMutation detail == Just "invalid body")
            CheckPassed -> assert False

prop_negative_properties_enabled :: Property
prop_negative_properties_enabled =
    property $ do
        let op = emptyOperation
            config = defaultTestConfig{tcNegativeTesting = True}
            dummyExecutor _timeout _req = pure (ApiResponse 400 [] "" 0)
            props = propertiesForSpecWithConfig mempty config dummyExecutor [op]
        assert (any (T.isPrefixOf "NEGATIVE:" . fst) props)

prop_negative_properties_disabled :: Property
prop_negative_properties_disabled =
    property $ do
        let op = emptyOperation
            config = defaultTestConfig{tcNegativeTesting = False}
            dummyExecutor _timeout _req = pure (ApiResponse 400 [] "" 0)
            props = propertiesForSpecWithConfig mempty config dummyExecutor [op]
        assert (not (any (T.isPrefixOf "NEGATIVE:" . fst) props))
