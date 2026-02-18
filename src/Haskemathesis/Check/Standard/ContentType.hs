{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.Check.Standard.ContentType
Description : Content-Type header conformance validation
Stability   : experimental

This module provides checks for validating that API responses include
a Content-Type header that matches the documented media types in the
OpenAPI specification.
-}
module Haskemathesis.Check.Standard.ContentType (contentTypeConformance) where

import qualified Data.Map.Strict as Map
import Data.Text.Encoding (decodeUtf8)
import Haskemathesis.Check.Standard.Helpers (failureDetail, lookupHeader, matchesContentType, responseSchemasForStatus)
import Haskemathesis.Check.Types (Check (..), CheckResult (..))
import Haskemathesis.Execute.Types (ApiResponse (..))
import Haskemathesis.OpenApi.Types (ResponseSpec (..))
import Network.HTTP.Types (hContentType)

{- | Check that the response Content-Type header matches documented media types.

This check validates that every response includes a Content-Type header
when expected, and that the content type matches one of the media types
documented in the OpenAPI specification for that status code.

==== Behavior

* __Passes__ when no response schema is defined for the status code
* __Passes__ when the response content map is empty (no media types documented)
* __Passes__ when the Content-Type matches a documented media type
* __Fails__ when the response is missing a Content-Type header
* __Fails__ when the Content-Type doesn't match any documented media type

==== Media type matching

The check performs flexible media type matching, accounting for:

* Character set parameters (e.g., @application\/json; charset=utf-8@)
* Media type wildcards in the specification

==== Example

@
-- Include content type validation in your checks
checks = ['contentTypeConformance', 'responseSchemaConformance']
@
-}
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
