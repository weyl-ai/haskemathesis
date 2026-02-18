{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{- | Authentication configuration and request mutation helpers.

This module provides types and functions for configuring authentication
in Haskemathesis tests. It supports various authentication schemes
including HTTP Basic, Bearer tokens, and API keys.

=== Supported Authentication Types

* 'AuthBasic' - HTTP Basic authentication (username/password)
* 'AuthBearer' - Bearer token authentication (OAuth2, JWT, etc.)
* 'AuthApiKey' - API key authentication (header, query, or cookie)

=== Basic Usage

@
import Haskemathesis.Auth.Config (AuthConfig(..), AuthValue(..), applyAuthForOperation)
import Haskemathesis.Config (defaultTestConfig)

-- Create auth configuration
authConfig :: AuthConfig
authConfig = AuthConfig $ Map.fromList
    [ ("bearerAuth", AuthBearer "my-token-123")
    , ("basicAuth", AuthBasic "username" "password")
    ]

-- Use in test configuration
config = defaultTestConfig { tcAuthConfig = Just authConfig }
@
-}
module Haskemathesis.Auth.Config (
    AuthConfig (..),
    AuthValue (..),
    applyAuthForOperation,
)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))
import Network.HTTP.Types (HeaderName)

{- | User-provided auth values keyed by security scheme name.

The 'AuthConfig' type wraps a map from security scheme names (as defined
in the OpenAPI spec) to their corresponding 'AuthValue's. The scheme
names must match those defined in the OpenAPI specification's
@securitySchemes@ section.

=== Example

@
authConfig :: AuthConfig
authConfig = AuthConfig $ Map.fromList
    [ ("bearerAuth", AuthBearer "my-token")
    , ("basicAuth", AuthBasic "user" "pass")
    , ("apiKey", AuthApiKey "secret-key")
    ]
@
-}
newtype AuthConfig = AuthConfig
    { authValues :: Map Text AuthValue
    }
    deriving (Eq, Show)

{- | Authentication values supplied by users.

This type represents the different kinds of authentication credentials
that can be provided. The constructor used must match the security
scheme type defined in the OpenAPI specification.

=== Constructors

* 'AuthBearer' - Bearer token (for HTTP Bearer auth)
* 'AuthBasic' - Username and password (for HTTP Basic auth)
* 'AuthApiKey' - API key string (for API key auth)

=== Example

@
-- Bearer token (OAuth2, JWT, etc.)
bearerAuth :: AuthValue
bearerAuth = AuthBearer "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."

-- Basic authentication
basicAuth :: AuthValue
basicAuth = AuthBasic "username" "password"

-- API key
apiKeyAuth :: AuthValue
apiKeyAuth = AuthApiKey "my-secret-api-key"
@
-}
data AuthValue
    = AuthBearer !Text
    | AuthBasic !Text !Text
    | AuthApiKey !Text
    deriving (Eq, Show)

{- | Apply authentication to a request for a specific operation.

This function examines the security requirements of the given operation
and applies the appropriate authentication credentials from the
'AuthConfig'. It supports multiple authentication schemes and will
use the first one that matches both the operation's requirements and
the available credentials.

=== Parameters

* @openApi@ - The OpenAPI specification (contains security scheme definitions)
* @config@ - The 'AuthConfig' containing user credentials
* @op@ - The 'ResolvedOperation' being tested
* @req@ - The 'ApiRequest' to add authentication to

=== Return Value

Returns the request with authentication headers/query parameters added.
If no authentication is required or available, the request is returned
unchanged.

=== Example

@
import Haskemathesis.Auth.Config (AuthConfig(..), AuthValue(..), applyAuthForOperation)
import Data.OpenApi (OpenApi)

addAuth :: OpenApi -> ApiRequest -> ResolvedOperation -> ApiRequest
addAuth spec req op =
    let authConfig = AuthConfig $ Map.fromList [("bearerAuth", AuthBearer "my-token")]
    in applyAuthForOperation spec authConfig op req
@
-}
applyAuthForOperation :: OpenApi -> AuthConfig -> ResolvedOperation -> ApiRequest -> ApiRequest
applyAuthForOperation openApi config op req =
    case selectAuthForOperation openApi config op of
        Nothing -> req
        Just auths -> foldl' applyAuth req auths

applyAuth :: ApiRequest -> AppliedAuth -> ApiRequest
applyAuth req auth =
    case auth of
        ApplyHeader name value ->
            req{reqHeaders = (name, value) : reqHeaders req}
        ApplyQuery name value ->
            req{reqQueryParams = (name, value) : reqQueryParams req}

selectAuthForOperation :: OpenApi -> AuthConfig -> ResolvedOperation -> Maybe [AppliedAuth]
selectAuthForOperation openApi config op =
    if null (roSecurity op)
        then Nothing
        else
            let schemes = securitySchemesMap (_openApiComponents openApi)
             in findSatisfied schemes (roSecurity op) config

findSatisfied :: Map Text SecurityScheme -> [SecurityRequirement] -> AuthConfig -> Maybe [AppliedAuth]
findSatisfied schemes requirements config =
    findJust (map (applyRequirement schemes config) requirements)

applyRequirement :: Map Text SecurityScheme -> AuthConfig -> SecurityRequirement -> Maybe [AppliedAuth]
applyRequirement schemes config (SecurityRequirement requirement) =
    traverse (resolveScheme schemes config) (InsOrdHashMap.toList requirement)
        >>= (Just . concat)

resolveScheme :: Map Text SecurityScheme -> AuthConfig -> (Text, [Text]) -> Maybe [AppliedAuth]
resolveScheme schemes config (schemeName, _scopes) = do
    scheme <- Map.lookup schemeName schemes
    authValue <- Map.lookup schemeName (authValues config)
    applyScheme scheme authValue

applyScheme :: SecurityScheme -> AuthValue -> Maybe [AppliedAuth]
applyScheme (SecurityScheme schemeType _desc) authValue =
    case (schemeType, authValue) of
        (SecuritySchemeHttp HttpSchemeBasic, AuthBasic username password) ->
            Just [ApplyHeader "Authorization" (basicHeader username password)]
        (SecuritySchemeHttp (HttpSchemeBearer _), AuthBearer token) ->
            Just [ApplyHeader "Authorization" (bearerHeader token)]
        (SecuritySchemeApiKey (ApiKeyParams name location), AuthApiKey value) ->
            Just (apiKeyAuth location name value)
        _otherScheme -> Nothing

apiKeyAuth :: ApiKeyLocation -> Text -> Text -> [AppliedAuth]
apiKeyAuth location name value =
    case location of
        ApiKeyHeader -> [ApplyHeader (toHeaderName name) (encodeUtf8 value)]
        ApiKeyQuery -> [ApplyQuery name value]
        ApiKeyCookie -> [ApplyHeader "Cookie" (encodeUtf8 (name <> "=" <> value))]

basicHeader :: Text -> Text -> ByteString
basicHeader username password =
    let raw = encodeUtf8 (username <> ":" <> password)
     in BS8.concat ["Basic ", Base64.encode raw]

bearerHeader :: Text -> ByteString
bearerHeader token =
    BS8.concat ["Bearer ", encodeUtf8 token]

toHeaderName :: Text -> HeaderName
toHeaderName = CI.mk . encodeUtf8

securitySchemesMap :: Components -> Map Text SecurityScheme
securitySchemesMap components =
    case _componentsSecuritySchemes components of
        SecurityDefinitions defs -> Map.fromList (InsOrdHashMap.toList defs)

findJust :: [Maybe a] -> Maybe a
findJust = findMaybe

findMaybe :: [Maybe a] -> Maybe a
findMaybe [] = Nothing
findMaybe (x : xs) =
    case x of
        Just v -> Just v
        Nothing -> findMaybe xs

-- Internal representation for applying auth to a request.
data AppliedAuth
    = ApplyHeader !HeaderName !ByteString
    | ApplyQuery !Text !Text
