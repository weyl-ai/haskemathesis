{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Negative.Generation (spec) where

import Data.Maybe (isNothing)
import Hedgehog (Property, assert, forAll, property, success)
import Test.Hspec (Spec, describe)

import Haskemathesis.Gen.Negative (genNegativeRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..), ResolvedRequestBody (..))
import Haskemathesis.Schema
import Haskemathesis.Test.Support (emptyOperation, itProp)

spec :: Spec
spec =
    describe "Negative request generation" $ do
        itProp "negative request generator produces mutation when possible" prop_negative_request_generated
        itProp "negative request generator returns Nothing when no candidates" prop_negative_request_none_when_no_candidates

prop_negative_request_generated :: Property
prop_negative_request_generated =
    property $ do
        let op =
                emptyOperation
                    { roRequestBody =
                        Just
                            ResolvedRequestBody
                                { rbContentType = "application/json"
                                , rbSchema = emptySchema{schemaType = Just SString}
                                }
                    }
        mReq <- forAll (genNegativeRequest op)
        case mReq of
            Nothing -> assert False
            Just _ -> success

prop_negative_request_none_when_no_candidates :: Property
prop_negative_request_none_when_no_candidates =
    property $ do
        let op = emptyOperation
        mReq <- forAll (genNegativeRequest op)
        assert (isNothing mReq)
