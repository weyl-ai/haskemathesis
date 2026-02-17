{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Property (
    propertyForOperation,
    propertiesForSpec,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, annotate, evalIO, failure, forAll, property, success)
import Hedgehog.Internal.Property (PropertyT)
import System.Environment (lookupEnv)

import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse, BaseUrl)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Report.Render (renderFailureDetail)

propertyForOperation ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Property
propertyForOperation mBase checks execute op =
    property $ do
        req <- forAll (genApiRequest op)
        res <- evalIO (execute req)
        runChecks mBase checks req res op

propertiesForSpec ::
    Maybe BaseUrl ->
    [Check] ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpec mBase checks execute ops =
    [ (operationLabel op, propertyForOperation mBase checks execute op)
    | op <- ops
    ]

runChecks ::
    Maybe BaseUrl ->
    [Check] ->
    ApiRequest ->
    ApiResponse ->
    ResolvedOperation ->
    PropertyT IO ()
runChecks mBase checks req res op =
    case firstFailure of
        Nothing -> success
        Just detail -> do
            mSeed <- evalIO (lookupEnv "HEDGEHOG_SEED")
            let seedText = T.pack <$> mSeed
            annotate (T.unpack (renderFailureDetail mBase seedText detail))
            failure
  where
    firstFailure =
        case [detail | Check{checkRun = run} <- checks, CheckFailed detail <- [run req res op]] of
            [] -> Nothing
            (detail : _) -> Just detail

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
