{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Check.Standard.ContentType (contentTypeConformance) where

import qualified Data.Map.Strict as Map
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (hContentType)

import Haskemathesis.Check.Standard.Helpers (failureDetail, lookupHeader, matchesContentType, responseSchemasForStatus)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResponseSpec (..))

contentTypeConformance :: Check
contentTypeConformance =
    Check "content_type_conformance" $ \req res op ->
        case responseSchemasForStatus (resStatusCode res) op of
            Nothing -> CheckPassed
            Just responseSpec
                | Map.null (rsContent responseSpec) -> CheckPassed
                | otherwise ->
                    case lookupHeader hContentType (resHeaders res) of
                        Nothing ->
                            CheckFailed
                                ( failureDetail
                                    "content_type_conformance"
                                    "response is missing Content-Type header"
                                    []
                                    Nothing
                                    req
                                    res
                                    op
                                )
                        Just rawType ->
                            let contentType = decodeUtf8 rawType
                             in if matchesContentType contentType (rsContent responseSpec)
                                    then CheckPassed
                                    else
                                        CheckFailed
                                            ( failureDetail
                                                "content_type_conformance"
                                                ("response Content-Type not documented: " <> contentType)
                                                []
                                                Nothing
                                                req
                                                res
                                                op
                                            )
