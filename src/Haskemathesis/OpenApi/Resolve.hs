{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Haskemathesis.OpenApi.Resolve
Description : OpenAPI specification resolution
Stability   : experimental

This module provides functionality to resolve an OpenAPI specification
into a list of normalized operations that can be used for testing.
It handles reference resolution, parameter extraction, and response
schema mapping.
-}
module Haskemathesis.OpenApi.Resolve (
    resolveOperations,
)
where

import Data.Aeson ()
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.HashSet.InsOrd as InsOrdHashSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.OpenApi (
    Components (..),
    Header (..),
    MediaTypeObject (..),
    OpenApi (..),
    Operation (..),
    Param (..),
    ParamLocation (..),
    PathItem (..),
    Reference (..),
    Referenced (..),
    RequestBody (..),
    Response (..),
    Responses (..),
    Schema,
    SecurityRequirement,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Haskemathesis.OpenApi.Convert (convertSchema)
import qualified Haskemathesis.OpenApi.Types as HOT
import qualified Haskemathesis.Schema as HS
import Network.HTTP.Media (MediaType, renderHeader)

{- | Resolve an OpenAPI specification into a list of testable operations.

This function extracts all operations (GET, POST, PUT, DELETE, etc.) from
an OpenAPI specification and resolves all @$ref@ references to produce
a flat list of 'ResolvedOperation' values ready for testing.

==== Resolution process

1. Iterates through all paths in the specification
2. Extracts operations for each HTTP method defined on each path
3. Resolves @$ref@ references in parameters, request bodies, and responses
4. Converts openapi3 schemas to internal 'Schema' representation
5. Applies path-level parameters to all operations

==== Security handling

Operations inherit security requirements from the global level unless
they define their own security requirements.

==== Example

@
import Data.OpenApi (OpenApi)
import Haskemathesis.OpenApi.Resolve

-- Load and resolve operations
operations :: [ResolvedOperation]
operations = resolveOperations myOpenApiSpec

-- Filter to specific operations
userOps = filter (\\op -> "users" \`elem\` roTags op) operations
@
-}
resolveOperations :: OpenApi -> [HOT.ResolvedOperation]
resolveOperations openApi =
    concatMap (uncurry (resolvePathItem components globalSecurity)) (InsOrdHashMap.toList pathMap)
  where
    components = _openApiComponents openApi
    globalSecurity = _openApiSecurity openApi
    pathMap = _openApiPaths openApi

resolvePathItem :: Components -> [SecurityRequirement] -> FilePath -> PathItem -> [HOT.ResolvedOperation]
resolvePathItem components globalSecurity path item =
    catMaybes
        [ resolveOperation components globalSecurity path (T.pack "GET") (_pathItemGet item)
        , resolveOperation components globalSecurity path (T.pack "PUT") (_pathItemPut item)
        , resolveOperation components globalSecurity path (T.pack "POST") (_pathItemPost item)
        , resolveOperation components globalSecurity path (T.pack "DELETE") (_pathItemDelete item)
        , resolveOperation components globalSecurity path (T.pack "OPTIONS") (_pathItemOptions item)
        , resolveOperation components globalSecurity path (T.pack "HEAD") (_pathItemHead item)
        , resolveOperation components globalSecurity path (T.pack "PATCH") (_pathItemPatch item)
        , resolveOperation components globalSecurity path (T.pack "TRACE") (_pathItemTrace item)
        ]
  where
    pathParams = _pathItemParameters item

    resolveOperation comps globalSec p method mOp = do
        op <- mOp
        let params = resolveParams comps (pathParams <> _operationParameters op)
        let requestBody = resolveRequestBody comps (_operationRequestBody op)
        let (responses, defaultResponse) = resolveResponses comps (_operationResponses op)
        let tags = InsOrdHashSet.toList (_operationTags op)
        let security = resolveSecurity globalSec (_operationSecurity op)
        pure
            HOT.ResolvedOperation
                { HOT.roMethod = method
                , HOT.roPath = T.pack p
                , HOT.roOperationId = _operationOperationId op
                , HOT.roTags = tags
                , HOT.roParameters = params
                , HOT.roRequestBody = requestBody
                , HOT.roResponses = responses
                , HOT.roDefaultResponse = defaultResponse
                , HOT.roSecurity = security
                }

resolveSecurity :: [SecurityRequirement] -> [SecurityRequirement] -> [SecurityRequirement]
resolveSecurity globalSecurity operationSecurity =
    if null operationSecurity
        then globalSecurity
        else operationSecurity

resolveParams :: Components -> [Referenced Param] -> [HOT.ResolvedParam]
resolveParams components =
    mapMaybe (resolveParam components)

resolveParam :: Components -> Referenced Param -> Maybe HOT.ResolvedParam
resolveParam components ref = do
    param <- resolveReferencedParam components ref
    schemaRef <- _paramSchema param
    schema <- resolveReferencedSchema components schemaRef
    pure
        HOT.ResolvedParam
            { HOT.rpName = _paramName param
            , HOT.rpLocation = convertLocation (_paramIn param)
            , HOT.rpRequired = fromMaybe False (_paramRequired param)
            , HOT.rpSchema = convertSchema (resolveSchemaRef components) schema
            }

resolveRequestBody :: Components -> Maybe (Referenced RequestBody) -> Maybe HOT.ResolvedRequestBody
resolveRequestBody components mRef = do
    ref <- mRef
    body <- resolveReferencedRequestBody components ref
    pickRequestBody components body

resolveResponses :: Components -> Responses -> (Map Int HOT.ResponseSpec, Maybe HOT.ResponseSpec)
resolveResponses components responses =
    (statusResponses, defaultResponse)
  where
    statusResponses =
        Map.fromList
            (mapMaybe (resolveStatusResponse components) (InsOrdHashMap.toList (_responsesResponses responses)))
    defaultResponse = do
        responseRef <- _responsesDefault responses
        response <- resolveReferencedResponse components responseRef
        pure (resolveResponseSpec components response)

resolveStatusResponse :: Components -> (Int, Referenced Response) -> Maybe (Int, HOT.ResponseSpec)
resolveStatusResponse components (statusCode, ref) = do
    response <- resolveReferencedResponse components ref
    pure (statusCode, resolveResponseSpec components response)

resolveResponseSpec :: Components -> Response -> HOT.ResponseSpec
resolveResponseSpec components response =
    HOT.ResponseSpec
        { HOT.rsContent =
            Map.fromList (mapMaybe toEntry (InsOrdHashMap.toList (_responseContent response)))
        , HOT.rsHeaders =
            Map.fromList (mapMaybe (resolveHeaderSchema components) (InsOrdHashMap.toList (_responseHeaders response)))
        , HOT.rsRequiredHeaders =
            resolveRequiredHeaders components (InsOrdHashMap.toList (_responseHeaders response))
        }
  where
    toEntry (mediaType, mediaTypeObject) = do
        schemaRef <- _mediaTypeObjectSchema mediaTypeObject
        schema <- resolveReferencedSchema components schemaRef
        pure (renderMediaType mediaType, convertSchema (resolveSchemaRef components) schema)

pickRequestBody :: Components -> RequestBody -> Maybe HOT.ResolvedRequestBody
pickRequestBody components body =
    case InsOrdHashMap.toList (_requestBodyContent body) of
        [] -> Nothing
        ((mediaType, mediaTypeObject) : _) -> do
            schemaRef <- _mediaTypeObjectSchema mediaTypeObject
            schema <- resolveReferencedSchema components schemaRef
            pure
                HOT.ResolvedRequestBody
                    { HOT.rbContentType = renderMediaType mediaType
                    , HOT.rbSchema = convertSchema (resolveSchemaRef components) schema
                    }

convertLocation :: ParamLocation -> HOT.ParamLocation
convertLocation location =
    case location of
        ParamPath -> HOT.ParamPath
        ParamQuery -> HOT.ParamQuery
        ParamHeader -> HOT.ParamHeader
        ParamCookie -> HOT.ParamCookie

-- | Resolve a referenced value, returning the inline value or looking up the reference.
resolveReferenced :: (Reference -> Maybe a) -> Referenced a -> Maybe a
resolveReferenced resolveRef ref =
    case ref of
        Inline x -> Just x
        Ref r -> resolveRef r

-- | Resolve a component reference by section name and accessor.
resolveComponentRef :: Text -> (Components -> InsOrdHashMap.InsOrdHashMap Text a) -> Components -> Reference -> Maybe a
resolveComponentRef section accessor components (Reference refText) = do
    name <- refName section refText
    InsOrdHashMap.lookup name (accessor components)

resolveReferencedSchema :: Components -> Referenced Schema -> Maybe Schema
resolveReferencedSchema components = resolveReferenced (resolveSchemaRef components)

resolveReferencedParam :: Components -> Referenced Param -> Maybe Param
resolveReferencedParam components = resolveReferenced (resolveParamRef components)

resolveReferencedRequestBody :: Components -> Referenced RequestBody -> Maybe RequestBody
resolveReferencedRequestBody components = resolveReferenced (resolveRequestBodyRef components)

resolveReferencedResponse :: Components -> Referenced Response -> Maybe Response
resolveReferencedResponse components = resolveReferenced (resolveResponseRef components)

resolveReferencedHeader :: Components -> Referenced Header -> Maybe Header
resolveReferencedHeader components = resolveReferenced (resolveHeaderRef components)

resolveSchemaRef :: Components -> Reference -> Maybe Schema
resolveSchemaRef = resolveComponentRef "schemas" _componentsSchemas

resolveParamRef :: Components -> Reference -> Maybe Param
resolveParamRef = resolveComponentRef "parameters" _componentsParameters

resolveRequestBodyRef :: Components -> Reference -> Maybe RequestBody
resolveRequestBodyRef = resolveComponentRef "requestBodies" _componentsRequestBodies

resolveResponseRef :: Components -> Reference -> Maybe Response
resolveResponseRef = resolveComponentRef "responses" _componentsResponses

resolveHeaderRef :: Components -> Reference -> Maybe Header
resolveHeaderRef = resolveComponentRef "headers" _componentsHeaders

refName :: Text -> Text -> Maybe Text
refName section refText =
    case T.splitOn "/" refText of
        ["#", "components", sectionName, name]
            | sectionName == section -> Just name
        _otherParts -> Nothing

renderMediaType :: MediaType -> Text
renderMediaType = decodeUtf8 . renderHeader

resolveRequiredHeaders :: Components -> [(Text, Referenced Header)] -> [Text]
resolveRequiredHeaders components =
    mapMaybe (requiredHeaderName components)

requiredHeaderName :: Components -> (Text, Referenced Header) -> Maybe Text
requiredHeaderName components (headerName, ref) = do
    header <- resolveReferencedHeader components ref
    required <- _headerRequired header
    if required
        then Just headerName
        else Nothing

resolveHeaderSchema :: Components -> (Text, Referenced Header) -> Maybe (Text, HS.Schema)
resolveHeaderSchema components (headerName, ref) = do
    header <- resolveReferencedHeader components ref
    schemaRef <- _headerSchema header
    schema <- resolveReferencedSchema components schemaRef
    pure (headerName, convertSchema (resolveSchemaRef components) schema)
