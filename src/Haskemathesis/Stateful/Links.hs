{-# LANGUAGE OverloadedStrings #-}

{- | OpenAPI Links parsing and resolution.

This module extracts link definitions from OpenAPI specs and converts them
into our internal OperationLink representation for stateful testing.

OpenAPI links define relationships between operations, specifying how the
response from one operation can provide parameters for another operation.
-}
module Haskemathesis.Stateful.Links (
    -- * Link extraction
    extractLinks,
    extractLinksFromOperation,

    -- * Expression parsing
    parseExpression,
    parseRuntimeExpression,

    -- * Target resolution
    resolveOperationId,
    findOperationById,

    -- * Helpers
    expressionOrValueToSource,
    decodeJsonPointer,
    parseOperationRef,
) where

import Data.Aeson (Value (String))
import qualified Data.HashMap.Strict.InsOrd as IOHM
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.OpenApi (
    Components (..),
    Link (..),
    OpenApi (..),
    Operation (..),
    PathItem (..),
    Reference (..),
    Referenced (..),
    Response (..),
    Responses (..),
 )
import Data.OpenApi.Internal (ExpressionOrValue (..))
import Data.Text (Text)
import qualified Data.Text as T

import Haskemathesis.Stateful.Types (
    OperationLink (..),
    ParameterBinding (..),
    ValueSource (..),
 )

{- | Extract all links from an OpenAPI specification.

This traverses all paths, operations, and responses to find link definitions.
Each link is converted to an OperationLink with the source operation
identified by path and method.
-}
extractLinks :: OpenApi -> [OperationLink]
extractLinks spec = concat $ do
    (path, pathItem) <- IOHM.toList (_openApiPaths spec)
    (method, op) <- getOperationsWithMethod pathItem
    pure $ extractLinksFromOperation spec (T.pack path) method op

-- | Get all operations from a PathItem with their HTTP methods.
getOperationsWithMethod :: PathItem -> [(Text, Operation)]
getOperationsWithMethod pathItem =
    catMaybes
        [ (,) "GET" <$> _pathItemGet pathItem
        , (,) "PUT" <$> _pathItemPut pathItem
        , (,) "POST" <$> _pathItemPost pathItem
        , (,) "DELETE" <$> _pathItemDelete pathItem
        , (,) "OPTIONS" <$> _pathItemOptions pathItem
        , (,) "HEAD" <$> _pathItemHead pathItem
        , (,) "PATCH" <$> _pathItemPatch pathItem
        , (,) "TRACE" <$> _pathItemTrace pathItem
        ]

-- | Extract links from a single operation.
extractLinksFromOperation ::
    OpenApi ->
    -- | Path
    Text ->
    -- | Method
    Text ->
    Operation ->
    [OperationLink]
extractLinksFromOperation spec path method op =
    let responses = _operationResponses op
        sourceOpId = _operationOperationId op
        sourceLabel = fromMaybe (method <> " " <> path) sourceOpId
        allResponses = getAllResponses spec responses
     in concatMap (extractLinksFromResponse spec sourceLabel) allResponses

-- | Get all responses from a Responses object, resolving references.
getAllResponses :: OpenApi -> Responses -> [Response]
getAllResponses spec responses =
    let defaultResp = maybeToList $ resolveResponse spec =<< _responsesDefault responses
        codeResponses = mapMaybe (resolveResponse spec) $ IOHM.elems (_responsesResponses responses)
     in defaultResp ++ codeResponses
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

-- | Resolve a Referenced Response to a Response.
resolveResponse :: OpenApi -> Referenced Response -> Maybe Response
resolveResponse _spec (Inline r) = Just r
resolveResponse spec (Ref ref) =
    let refName = T.drop 1 $ T.dropWhile (/= '/') $ T.dropWhile (/= '/') $ getRefText ref
     in IOHM.lookup refName (_componentsResponses $ _openApiComponents spec)
  where
    getRefText (Reference t) = t

-- | Extract links from a single response.
extractLinksFromResponse ::
    OpenApi ->
    -- | Source operation label
    Text ->
    Response ->
    [OperationLink]
extractLinksFromResponse spec sourceLabel response =
    mapMaybe (uncurry $ linkToOperationLink spec sourceLabel) $
        IOHM.toList $
            _responseLinks response

-- | Convert an OpenAPI Link to our OperationLink type.
linkToOperationLink ::
    OpenApi ->
    -- | Source operation label
    Text ->
    -- | Link name
    Text ->
    Referenced Link ->
    Maybe OperationLink
linkToOperationLink spec sourceLabel linkName refLink = do
    link <- resolveLink spec refLink
    targetOpId <- getTargetOperationId link
    let bindings = parseParameterBindings link
    pure
        OperationLink
            { olSourceOperation = sourceLabel
            , olTargetOperation = targetOpId
            , olParameterBindings = bindings
            , olDescription = _linkDescription link
            , olLinkName = Just linkName
            }

-- | Resolve a Referenced Link to a Link.
resolveLink :: OpenApi -> Referenced Link -> Maybe Link
resolveLink _spec (Inline l) = Just l
resolveLink spec (Ref ref) =
    let refName = T.drop 1 $ T.dropWhile (/= '/') $ T.dropWhile (/= '/') $ getRefText ref
     in IOHM.lookup refName (_componentsLinks $ _openApiComponents spec)
  where
    getRefText (Reference t) = t

{- | Get the target operation ID from a Link.
Prefers operationId over operationRef.
-}
getTargetOperationId :: Link -> Maybe Text
getTargetOperationId link =
    _linkOperationId link <|> parseOperationRef (_linkOperationRef link)
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) Nothing b = b
    (<|>) a _ = a

{- | Parse an operationRef to extract the operation path.
operationRef is a JSON Reference like "#/paths/~1users~1{userId}/get"
-}
parseOperationRef :: Maybe Text -> Maybe Text
parseOperationRef Nothing = Nothing
parseOperationRef (Just ref)
    | "#/paths/" `T.isPrefixOf` ref =
        -- Extract the path and method from the ref
        -- Format: #/paths/~1users~1{userId}/get
        let withoutPrefix = T.drop 8 ref -- Drop "#/paths/"
            parts = T.splitOn "/" withoutPrefix
         in case parts of
                [encodedPath, method] ->
                    let path = decodeJsonPointer encodedPath
                     in Just $ T.toUpper method <> " " <> path
                _other -> Nothing
    | otherwise = Nothing

{- | Decode a JSON Pointer path segment (RFC 6901).
~1 -> /, ~0 -> ~
-}
decodeJsonPointer :: Text -> Text
decodeJsonPointer = T.replace "~1" "/" . T.replace "~0" "~"

-- | Parse parameter bindings from a Link.
parseParameterBindings :: Link -> [ParameterBinding]
parseParameterBindings link =
    map (uncurry parseBinding) $ IOHM.toList (_linkParameters link)
  where
    parseBinding :: Text -> ExpressionOrValue -> ParameterBinding
    parseBinding paramName exprOrVal =
        ParameterBinding
            { pbTargetParam = paramName
            , pbSource = expressionOrValueToSource exprOrVal
            }

-- | Convert an ExpressionOrValue to a ValueSource.
expressionOrValueToSource :: ExpressionOrValue -> ValueSource
expressionOrValueToSource (Expression expr) = parseExpression expr
expressionOrValueToSource (Value val) = Literal val

{- | Parse a runtime expression string into a ValueSource.

Supports:
- $response.body#/path/to/field - Extract from response body
- $response.header.X-Header - Extract from response header
- $request.path.paramName - Use request path parameter
- $request.query.paramName - Use request query parameter
- $request.body#/path - Extract from request body
- $url - The URL of the operation
- Plain text - Treated as a literal
-}
parseExpression :: Text -> ValueSource
parseExpression expr
    | "$response.body#" `T.isPrefixOf` expr =
        FromResponseBody expr
    | "$response.header." `T.isPrefixOf` expr =
        let header = T.drop 17 expr -- Length of "$response.header."
         in FromResponseHeader header
    | "$request.path." `T.isPrefixOf` expr =
        let param = T.drop 14 expr -- Length of "$request.path."
         in FromState ("path." <> param)
    | "$request.query." `T.isPrefixOf` expr =
        let param = T.drop 15 expr -- Length of "$request.query."
         in FromState ("query." <> param)
    | "$request.body#" `T.isPrefixOf` expr =
        FromState ("requestBody" <> T.drop 14 expr)
    | "$url" == expr =
        FromState "url"
    | "$" `T.isPrefixOf` expr =
        -- Other $ expressions we don't fully support yet
        FromState (T.drop 1 expr)
    | otherwise =
        -- Plain text is a literal string
        Literal (String expr)

{- | Parse a runtime expression and return the parsed components.
Returns (source, jsonPath) where source is one of "body", "header", "path", etc.
-}
parseRuntimeExpression :: Text -> Either Text (Text, Maybe Text)
parseRuntimeExpression expr
    | "$response.body#" `T.isPrefixOf` expr =
        -- Length of "$response.body#" is 15
        let pointer = T.drop 15 expr
         in Right ("response.body", Just pointer)
    | "$response.header." `T.isPrefixOf` expr =
        let header = T.drop 17 expr
         in Right ("response.header." <> header, Nothing)
    | "$request.path." `T.isPrefixOf` expr =
        let param = T.drop 14 expr
         in Right ("request.path." <> param, Nothing)
    | "$request.query." `T.isPrefixOf` expr =
        let param = T.drop 15 expr
         in Right ("request.query." <> param, Nothing)
    | "$request.body#" `T.isPrefixOf` expr =
        let pointer = T.drop 14 expr
         in Right ("request.body", Just pointer)
    | "$url" == expr =
        Right ("url", Nothing)
    | "$" `T.isPrefixOf` expr =
        Left $ "Unsupported runtime expression: " <> expr
    | otherwise =
        Left $ "Not a runtime expression: " <> expr

-- | Resolve an operationId to find the operation in the spec.
resolveOperationId :: OpenApi -> Text -> Maybe (Text, Text, Operation)
resolveOperationId spec opId = findOperationById opId spec

{- | Find an operation by its operationId.
Returns (path, method, operation) if found.
-}
findOperationById :: Text -> OpenApi -> Maybe (Text, Text, Operation)
findOperationById opId spec = go $ IOHM.toList (_openApiPaths spec)
  where
    go [] = Nothing
    go ((path, pathItem) : rest) =
        case findInPathItem opId pathItem of
            Just (method, op) -> Just (T.pack path, method, op)
            Nothing -> go rest

    findInPathItem :: Text -> PathItem -> Maybe (Text, Operation)
    findInPathItem targetId pItem =
        let ops = getOperationsWithMethod pItem
         in case filter (hasOperationId targetId . snd) ops of
                ((method, op) : _) -> Just (method, op)
                [] -> Nothing

    hasOperationId :: Text -> Operation -> Bool
    hasOperationId targetId op = _operationOperationId op == Just targetId
