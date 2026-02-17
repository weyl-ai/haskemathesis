{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Property (
    propertyForOperation,
    propertyForOperationWithConfig,
    propertiesForSpec,
    propertiesForSpecWithConfig,
) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, annotate, evalIO, failure, forAll, property, success, withTests)
import Hedgehog.Internal.Property (PropertyT)
import System.Environment (lookupEnv)

import Data.OpenApi (OpenApi)

import Haskemathesis.Auth.Config (applyAuthForOperation)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Config (TestConfig (..))
import Haskemathesis.Execute.Types (ApiRequest, ApiResponse, BaseUrl)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Report.Render (renderFailureDetailAnsi)

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

propertyForOperationWithConfig ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    ResolvedOperation ->
    Property
propertyForOperationWithConfig openApi config execute op =
    withTests (fromIntegral (tcPropertyCount config)) $
        property $ do
            req <- forAll (genApiRequest op)
            let req' =
                    case tcAuthConfig config of
                        Nothing -> req
                        Just auth -> applyAuthForOperation openApi auth op req
            res <- evalIO (execute req')
            runChecks (tcBaseUrl config) (tcChecks config) req' res op

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

propertiesForSpecWithConfig ::
    OpenApi ->
    TestConfig ->
    (ApiRequest -> IO ApiResponse) ->
    [ResolvedOperation] ->
    [(Text, Property)]
propertiesForSpecWithConfig openApi config execute ops =
    [ (operationLabel op, propertyForOperationWithConfig openApi config execute op)
    | op <- ops
    , tcOperationFilter config op
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
            annotate (T.unpack (renderFailureDetailAnsi mBase seedText detail))
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
