module Haskemathesis.OpenApi.Resolve (
    resolveOperations,
) where

import Data.Aeson ()
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.HashSet.InsOrd as InsOrdHashSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Media (MediaType, renderHeader)

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

import Haskemathesis.OpenApi.Convert (convertSchema)
import qualified Haskemathesis.OpenApi.Types as HOT
import qualified Haskemathesis.Schema as HS

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

resolveReferencedSchema :: Components -> Referenced Schema -> Maybe Schema
resolveReferencedSchema components ref =
    case ref of
        Inline s -> Just s
        Ref r -> resolveSchemaRef components r

resolveReferencedParam :: Components -> Referenced Param -> Maybe Param
resolveReferencedParam components ref =
    case ref of
        Inline p -> Just p
        Ref r -> resolveParamRef components r

resolveReferencedRequestBody :: Components -> Referenced RequestBody -> Maybe RequestBody
resolveReferencedRequestBody components ref =
    case ref of
        Inline b -> Just b
        Ref r -> resolveRequestBodyRef components r

resolveReferencedResponse :: Components -> Referenced Response -> Maybe Response
resolveReferencedResponse components ref =
    case ref of
        Inline r -> Just r
        Ref r -> resolveResponseRef components r

resolveReferencedHeader :: Components -> Referenced Header -> Maybe Header
resolveReferencedHeader components ref =
    case ref of
        Inline h -> Just h
        Ref r -> resolveHeaderRef components r

resolveSchemaRef :: Components -> Reference -> Maybe Schema
resolveSchemaRef components (Reference refText) = do
    name <- refName (T.pack "schemas") refText
    InsOrdHashMap.lookup name (_componentsSchemas components)

resolveParamRef :: Components -> Reference -> Maybe Param
resolveParamRef components (Reference refText) = do
    name <- refName (T.pack "parameters") refText
    InsOrdHashMap.lookup name (_componentsParameters components)

resolveRequestBodyRef :: Components -> Reference -> Maybe RequestBody
resolveRequestBodyRef components (Reference refText) = do
    name <- refName (T.pack "requestBodies") refText
    InsOrdHashMap.lookup name (_componentsRequestBodies components)

resolveResponseRef :: Components -> Reference -> Maybe Response
resolveResponseRef components (Reference refText) = do
    name <- refName (T.pack "responses") refText
    InsOrdHashMap.lookup name (_componentsResponses components)

resolveHeaderRef :: Components -> Reference -> Maybe Header
resolveHeaderRef components (Reference refText) = do
    name <- refName (T.pack "headers") refText
    InsOrdHashMap.lookup name (_componentsHeaders components)

refName :: Text -> Text -> Maybe Text
refName section refText =
    case T.splitOn (T.pack "/") refText of
        [hash, componentsKey, sectionName, name]
            | hash == T.pack "#"
                && componentsKey == T.pack "components"
                && sectionName == section ->
                Just name
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
