{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Authentication configuration and request mutation helpers.
module Haskemathesis.Auth.Config (
    AuthConfig (..),
    AuthValue (..),
    applyAuthForOperation,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (HeaderName)

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
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

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

-- | User-provided auth values keyed by security scheme name.
newtype AuthConfig = AuthConfig
    { authValues :: Map Text AuthValue
    }
    deriving (Eq, Show)

-- | Auth values supplied by users.
data AuthValue
    = AuthBearer !Text
    | AuthBasic !Text !Text
    | AuthApiKey !Text
    deriving (Eq, Show)

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
