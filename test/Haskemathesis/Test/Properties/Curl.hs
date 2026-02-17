{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Curl (spec) where

import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, assert, property, (===))
import Test.Hspec (Spec, describe)

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.Report.Curl (toCurl)
import Haskemathesis.Test.Support (itProp)

spec :: Spec
spec =
    describe "Curl rendering" $ do
        itProp "includes content-type header when body present" prop_curl_includes_content_type_header_when_body
        itProp "respects existing content-type header" prop_curl_respects_existing_content_type_header
        itProp "renders query params" prop_request_query_params_rendered_in_curl
        itProp "renders duplicate query params" prop_request_query_params_duplicate_in_curl
        itProp "preserves query param order" prop_request_query_params_ordered_in_curl
        itProp "includes base url" prop_curl_includes_base_url
        itProp "omits body when none" prop_curl_omits_body_when_none
        itProp "encodes spaces in query" prop_curl_encodes_spaces_in_query
        itProp "encodes reserved chars in query" prop_curl_encodes_reserved_chars_in_query
        itProp "renders header values with spaces" prop_request_header_value_with_spaces_in_curl

prop_curl_includes_content_type_header_when_body :: Property
prop_curl_includes_content_type_header_when_body =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "POST"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Just ("application/json", encodeUtf8 "{}")
                    }
            curl = toCurl Nothing req
        assert ("-H 'Content-Type: application/json'" `T.isInfixOf` curl)
        assert ("-d '" `T.isInfixOf` curl)

prop_curl_respects_existing_content_type_header :: Property
prop_curl_respects_existing_content_type_header =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "POST"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = [(CI.mk "Content-Type", "application/json")]
                    , reqBody = Just ("application/json", encodeUtf8 "{}")
                    }
            curl = toCurl Nothing req
        T.count "Content-Type: application/json" curl === 1

prop_request_query_params_rendered_in_curl :: Property
prop_request_query_params_rendered_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "hello"), ("page", "1")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/search?q=hello&page=1" `T.isInfixOf` curl)

prop_request_query_params_duplicate_in_curl :: Property
prop_request_query_params_duplicate_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = [("tag", "a"), ("tag", "b")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/items?tag=a&tag=b" `T.isInfixOf` curl)

prop_request_query_params_ordered_in_curl :: Property
prop_request_query_params_ordered_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("a", "1"), ("b", "2"), ("a", "3")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/search?a=1&b=2&a=3" `T.isInfixOf` curl)

prop_curl_includes_base_url :: Property
prop_curl_includes_base_url =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl (Just "https://example.com") req
        assert ("https://example.com/items" `T.isInfixOf` curl)

prop_curl_omits_body_when_none :: Property
prop_curl_omits_body_when_none =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert (not ("-d " `T.isInfixOf` curl))

prop_curl_encodes_spaces_in_query :: Property
prop_curl_encodes_spaces_in_query =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "hello world")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert
            ( "/search?q=hello+world" `T.isInfixOf` curl
                || "/search?q=hello%20world" `T.isInfixOf` curl
            )

prop_curl_encodes_reserved_chars_in_query :: Property
prop_curl_encodes_reserved_chars_in_query =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/search"
                    , reqQueryParams = [("q", "a&b=c")]
                    , reqHeaders = []
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("/search?q=a%26b%3Dc" `T.isInfixOf` curl)

prop_request_header_value_with_spaces_in_curl :: Property
prop_request_header_value_with_spaces_in_curl =
    property $ do
        let req =
                ApiRequest
                    { reqMethod = "GET"
                    , reqPath = "/items"
                    , reqQueryParams = []
                    , reqHeaders = [(CI.mk "X-Note", "hello world")]
                    , reqBody = Nothing
                    }
            curl = toCurl Nothing req
        assert ("X-Note: hello world" `T.isInfixOf` curl)
