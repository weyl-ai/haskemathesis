{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Test.Properties.Auth (spec) where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Hedgehog (Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Data.OpenApi (
    ApiKeyLocation (..),
    ApiKeyParams (..),
    Components (..),
    HttpSchemeType (..),
    OpenApi (..),
    SecurityDefinitions (..),
    SecurityRequirement (..),
    SecurityScheme (..),
    SecuritySchemeType (..),
 )

import Haskemathesis.Auth.Config (AuthConfig (..), AuthValue (..), applyAuthForOperation)
import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Haskemathesis.Test.Support (dummyRequest, emptyOperation, itProp)

spec :: Spec
spec =
    describe "Auth" $ do
        itProp "bearer adds Authorization header" prop_auth_bearer_header
        itProp "basic adds Authorization header" prop_auth_basic_header
        itProp "api key header" prop_auth_api_key_header
        itProp "api key query" prop_auth_api_key_query
        itProp "api key cookie" prop_auth_api_key_cookie
        itProp "selects first satisfiable requirement" prop_auth_selects_first_satisfied
        itProp "no security leaves request unchanged" prop_auth_no_security_no_change
        itProp "unknown scheme leaves request unchanged" prop_auth_unknown_scheme_no_change

prop_auth_bearer_header :: Property
prop_auth_bearer_header =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) Nothing
            openApi = openApiWithSchemes [("bearerAuth", scheme)]
            op = emptyOperation{roSecurity = [securityRequirement "bearerAuth"]}
            config = AuthConfig (Map.fromList [("bearerAuth", AuthBearer token)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
            expected = (CI.mk "Authorization", BS8.concat ["Bearer ", encodeUtf8 token])
        assert (expected `elem` reqHeaders req')

prop_auth_basic_header :: Property
prop_auth_basic_header =
    property $ do
        username <- forAll (Gen.text (Range.linear 1 12) Gen.alphaNum)
        password <- forAll (Gen.text (Range.linear 1 12) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeHttp HttpSchemeBasic) Nothing
            openApi = openApiWithSchemes [("basicAuth", scheme)]
            op = emptyOperation{roSecurity = [securityRequirement "basicAuth"]}
            config = AuthConfig (Map.fromList [("basicAuth", AuthBasic username password)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
            raw = encodeUtf8 (username <> ":" <> password)
            expected = (CI.mk "Authorization", BS8.concat ["Basic ", Base64.encode raw])
        assert (expected `elem` reqHeaders req')

prop_auth_api_key_header :: Property
prop_auth_api_key_header =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeApiKey (ApiKeyParams "X-Api-Key" ApiKeyHeader)) Nothing
            openApi = openApiWithSchemes [("apiKey", scheme)]
            op = emptyOperation{roSecurity = [securityRequirement "apiKey"]}
            config = AuthConfig (Map.fromList [("apiKey", AuthApiKey token)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
            expected = (CI.mk "X-Api-Key", encodeUtf8 token)
        assert (expected `elem` reqHeaders req')

prop_auth_api_key_query :: Property
prop_auth_api_key_query =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeApiKey (ApiKeyParams "api_key" ApiKeyQuery)) Nothing
            openApi = openApiWithSchemes [("apiKey", scheme)]
            op = emptyOperation{roSecurity = [securityRequirement "apiKey"]}
            config = AuthConfig (Map.fromList [("apiKey", AuthApiKey token)])
            req = (dummyRequest op){reqQueryParams = [("page", "1")]}
            req' = applyAuthForOperation openApi config op req
        assert (("api_key", token) `elem` reqQueryParams req')
        assert (("page", "1") `elem` reqQueryParams req')

prop_auth_api_key_cookie :: Property
prop_auth_api_key_cookie =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeApiKey (ApiKeyParams "session" ApiKeyCookie)) Nothing
            openApi = openApiWithSchemes [("apiKey", scheme)]
            op = emptyOperation{roSecurity = [securityRequirement "apiKey"]}
            config = AuthConfig (Map.fromList [("apiKey", AuthApiKey token)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
            expected = (CI.mk "Cookie", encodeUtf8 ("session=" <> token))
        assert (expected `elem` reqHeaders req')

prop_auth_selects_first_satisfied :: Property
prop_auth_selects_first_satisfied =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let missingScheme = SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) Nothing
            bearerScheme = SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) Nothing
            openApi = openApiWithSchemes [("missing", missingScheme), ("bearer", bearerScheme)]
            op =
                emptyOperation
                    { roSecurity =
                        [ securityRequirement "missing"
                        , securityRequirement "bearer"
                        ]
                    }
            config = AuthConfig (Map.fromList [("bearer", AuthBearer token)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
            expected = (CI.mk "Authorization", BS8.concat ["Bearer ", encodeUtf8 token])
        assert (expected `elem` reqHeaders req')

prop_auth_no_security_no_change :: Property
prop_auth_no_security_no_change =
    property $ do
        token <- forAll (Gen.text (Range.linear 1 16) Gen.alphaNum)
        let scheme = SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) Nothing
            openApi = openApiWithSchemes [("bearer", scheme)]
            op = emptyOperation{roSecurity = []}
            config = AuthConfig (Map.fromList [("bearer", AuthBearer token)])
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
        assert (req' == req)

prop_auth_unknown_scheme_no_change :: Property
prop_auth_unknown_scheme_no_change =
    property $ do
        let openApi = openApiWithSchemes []
            op = emptyOperation{roSecurity = [securityRequirement "unknown"]}
            config = AuthConfig mempty
            req = dummyRequest op
            req' = applyAuthForOperation openApi config op req
        assert (req' == req)

securityRequirement :: Text -> SecurityRequirement
securityRequirement name =
    SecurityRequirement (InsOrdHashMap.fromList [(name, [])])

openApiWithSchemes :: [(Text, SecurityScheme)] -> OpenApi
openApiWithSchemes schemes =
    let defs = InsOrdHashMap.fromList schemes
        components = mempty{_componentsSecuritySchemes = SecurityDefinitions defs}
     in mempty{_openApiComponents = components}
