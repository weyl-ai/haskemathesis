{-# LANGUAGE OverloadedStrings #-}

{- | Heuristic link detection for stateful API testing.

This module infers links between API operations when explicit OpenAPI Links
are not defined. It uses heuristics based on:

1. __Path pattern matching__: @POST /users@ links to @GET /users/{id}@
2. __Response schema analysis__: Response field @id@ matches path param @{id}@
3. __Naming conventions__: @userId@ in response matches @user_id@ parameter

These heuristics enable stateful testing even for specs without explicit links.

=== Usage

@
import Haskemathesis.Stateful.Heuristics
import Haskemathesis.Stateful.Links (extractLinks)

-- Combine explicit and inferred links
allLinks :: OpenApi -> [ResolvedOperation] -> [OperationLink]
allLinks spec ops = extractLinks spec <> inferLinks ops
@
-}
module Haskemathesis.Stateful.Heuristics (
    -- * Combined Link Inference
    inferLinks,

    -- * Path-Based Inference
    inferLinksFromPaths,
    findRelatedOperations,
    extractBasePath,
    extractPathParams,

    -- * Schema-Based Inference
    inferLinksFromSchemas,
    matchResponseFieldsToParams,

    -- * Naming Normalization
    normalizeParamName,
    namesMatch,
) where

import Data.Char (isUpper, toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
    ResponseSpec (..),
 )
import Haskemathesis.Schema (Schema (..), SchemaType (..))
import Haskemathesis.Stateful.Types (
    OperationLink (..),
    ParameterBinding (..),
    ValueSource (..),
 )

{- | Infer all links from a list of operations using heuristics.

Combines path-based and schema-based inference, deduplicating results.

>>> inferLinks [postUsersOp, getUsersIdOp, deleteUsersIdOp]
[OperationLink "createUser" "getUser" [...], OperationLink "createUser" "deleteUser" [...]]
-}
inferLinks :: [ResolvedOperation] -> [OperationLink]
inferLinks ops = nubByKey linkKey $ inferLinksFromPaths ops <> inferLinksFromSchemas ops
  where
    -- Deduplicate by (source, target) pair using a Set for O(n log n)
    linkKey :: OperationLink -> (Text, Text)
    linkKey link = (olSourceOperation link, olTargetOperation link)

    nubByKey :: (Ord k) => (a -> k) -> [a] -> [a]
    nubByKey f = go Set.empty
      where
        go _seen [] = []
        go seen (x : xs)
            | Set.member (f x) seen = go seen xs
            | otherwise = x : go (Set.insert (f x) seen) xs

{- | Infer links based on path pattern matching.

Detects common REST patterns:

* @POST /resources@ -> @GET /resources/{id}@
* @POST /resources@ -> @PUT /resources/{id}@
* @POST /resources@ -> @DELETE /resources/{id}@
* @POST /resources@ -> @PATCH /resources/{id}@

Also handles nested resources like @POST /users/{userId}/posts@ ->
@GET /users/{userId}/posts/{id}@.

=== Algorithm

1. Find all "creator" operations (POST methods)
2. For each creator, find operations with same base path plus additional params
3. Create links with bindings from response body to path params
-}
inferLinksFromPaths :: [ResolvedOperation] -> [OperationLink]
inferLinksFromPaths ops = concatMap (linksFromCreator ops) creators
  where
    -- POST operations are typically resource creators
    creators = filter ((== "POST") . roMethod) ops

-- | Find links from a single creator operation to related operations.
linksFromCreator :: [ResolvedOperation] -> ResolvedOperation -> [OperationLink]
linksFromCreator allOps creator =
    mapMaybe (createLink creator) (findRelatedOperations allOps creator)

-- | Create a link from creator to target operation.
createLink :: ResolvedOperation -> ResolvedOperation -> Maybe OperationLink
createLink creator target = do
    let sourceId = operationLabel creator
        targetId = operationLabel target
        -- Find path params in target that aren't in creator
        creatorParams = extractPathParams (roPath creator)
        targetParams = extractPathParams (roPath target)
        newParams = filter (`notElem` creatorParams) targetParams
    -- Only create link if there are new params to bind
    if null newParams
        then Nothing
        else
            Just
                OperationLink
                    { olSourceOperation = sourceId
                    , olTargetOperation = targetId
                    , olParameterBindings = map createBinding newParams
                    , olDescription = Just $ "Inferred: " <> roMethod target <> " after POST"
                    , olLinkName = Nothing
                    }
  where
    -- Create a binding from response body field to parameter
    createBinding :: Text -> ParameterBinding
    createBinding paramName =
        ParameterBinding
            { pbTargetParam = paramName
            , pbSource = FromResponseBody $ "$response.body#/" <> normalizeForJsonPath paramName
            }

    -- Convert parameter name to JSON path field name
    -- E.g., "userId" stays as "userId", but we also try common patterns
    normalizeForJsonPath :: Text -> Text
    normalizeForJsonPath name
        -- If param ends with "Id" and has at least 3 characters (e.g., "xId")
        | "Id" `T.isSuffixOf` name && not (T.null (T.dropEnd 2 name)) =
            -- Keep original - extraction will try variations
            name
        | otherwise = name

{- | Find operations related to a creator by path pattern.

An operation is related if:

1. It has the same base path as the creator
2. It has additional path parameters
3. It's a different method (GET, PUT, DELETE, PATCH)

=== Example

For @POST /users@, related operations might include:

* @GET /users/{id}@
* @PUT /users/{id}@
* @DELETE /users/{id}@
* @PATCH /users/{id}@
-}
findRelatedOperations :: [ResolvedOperation] -> ResolvedOperation -> [ResolvedOperation]
findRelatedOperations allOps creator =
    filter isRelated allOps
  where
    creatorBase = extractBasePath (roPath creator)
    creatorParams = extractPathParams (roPath creator)

    isRelated :: ResolvedOperation -> Bool
    isRelated op =
        -- Different operation
        operationLabel op /= operationLabel creator
            &&
            -- Not another POST (typically)
            roMethod op /= "POST"
            &&
            -- Same base path
            extractBasePath (roPath op) == creatorBase
            &&
            -- Has more path params (indicating a specific resource)
            hasMoreParams (extractPathParams (roPath op)) creatorParams

    -- Check if first list is longer than second without using length
    hasMoreParams :: [a] -> [a] -> Bool
    hasMoreParams [] _ = False
    hasMoreParams (_ : _) [] = True
    hasMoreParams (_ : xs) (_ : ys) = hasMoreParams xs ys

{- | Extract the base path from a path template.

Removes path parameter segments to get the "collection" path.

>>> extractBasePath "/users/{id}"
"/users"

>>> extractBasePath "/users/{userId}/posts/{postId}"
"/users/{userId}/posts"

>>> extractBasePath "/users"
"/users"
-}
extractBasePath :: Text -> Text
extractBasePath path =
    let segments = T.splitOn "/" path
        -- Remove trailing param segments
        baseSegments = removeTrailingParams segments
     in T.intercalate "/" baseSegments
  where
    removeTrailingParams :: [Text] -> [Text]
    removeTrailingParams segs = case unsnoc segs of
        Nothing -> []
        Just (initSegs, lastSeg)
            | isParamSegment lastSeg -> removeTrailingParams initSegs
            | otherwise -> segs

    -- Safe version of (init, last) combined
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc [] = Nothing
    unsnoc xs = Just (Prelude.init xs, Prelude.last xs)

    isParamSegment :: Text -> Bool
    isParamSegment seg = "{" `T.isPrefixOf` seg && "}" `T.isSuffixOf` seg

{- | Extract path parameter names from a path template.

>>> extractPathParams "/users/{id}"
["id"]

>>> extractPathParams "/users/{userId}/posts/{postId}"
["userId", "postId"]

>>> extractPathParams "/users"
[]
-}
extractPathParams :: Text -> [Text]
extractPathParams path =
    mapMaybe extractParam $ T.splitOn "/" path
  where
    extractParam :: Text -> Maybe Text
    extractParam seg
        | "{" `T.isPrefixOf` seg && "}" `T.isSuffixOf` seg =
            Just $ T.drop 1 $ T.dropEnd 1 seg
        | otherwise = Nothing

{- | Infer links based on response schema field matching.

Analyzes response schemas to find fields that match path parameters
in other operations. This handles cases where:

* Response has @id@ field -> matches @{id}@ parameter
* Response has @userId@ field -> matches @{user_id}@ parameter
* Response has @resourceId@ field -> matches @{resourceId}@ parameter

=== Algorithm

1. For each operation with a response schema containing object fields
2. Find operations with path parameters matching those field names
3. Create links with bindings from response fields to parameters
-}
inferLinksFromSchemas :: [ResolvedOperation] -> [OperationLink]
inferLinksFromSchemas ops = concatMap (linksFromResponseSchema ops) ops

-- | Find links from an operation based on its response schema.
linksFromResponseSchema :: [ResolvedOperation] -> ResolvedOperation -> [OperationLink]
linksFromResponseSchema allOps sourceOp =
    case getResponseFields sourceOp of
        [] -> []
        fields -> concatMap (findMatchingTargets sourceOp fields) allOps

-- | Get field names from the operation's success response schema.
getResponseFields :: ResolvedOperation -> [Text]
getResponseFields op =
    -- Look for 200, 201 responses
    maybe [] getSchemaFields lookup200or201
  where
    responses = roResponses op

    lookup200or201 :: Maybe Schema
    lookup200or201 =
        -- Try 201 first (common for POST), then 200
        case (Map.lookup 201 responses, Map.lookup 200 responses) of
            (Just spec, _) -> firstSchema spec
            (Nothing, Just spec) -> firstSchema spec
            (Nothing, Nothing) -> Nothing

    firstSchema :: ResponseSpec -> Maybe Schema
    firstSchema spec =
        -- Get the first content type's schema (prefer application/json)
        case Map.lookup "application/json" (rsContent spec) of
            Just s -> Just s
            Nothing -> case Map.elems (rsContent spec) of
                (s : _) -> Just s
                [] -> Nothing

    getSchemaFields :: Schema -> [Text]
    getSchemaFields schema = case schemaType schema of
        Just SObject -> Map.keys (schemaProperties schema)
        Just SString -> []
        Just SInteger -> []
        Just SNumber -> []
        Just SBoolean -> []
        Just SArray -> []
        Just SNull -> []
        Nothing -> []

-- | Find operations that can be linked from sourceOp based on field matches.
findMatchingTargets ::
    ResolvedOperation -> [Text] -> ResolvedOperation -> [OperationLink]
findMatchingTargets sourceOp responseFields targetOp
    | operationLabel sourceOp == operationLabel targetOp = []
    | otherwise =
        let targetPathParams = getPathParams targetOp
            matches = matchResponseFieldsToParams responseFields targetPathParams
         in [ OperationLink
                { olSourceOperation = operationLabel sourceOp
                , olTargetOperation = operationLabel targetOp
                , olParameterBindings =
                    [ ParameterBinding
                        { pbTargetParam = paramName
                        , pbSource = FromResponseBody $ "$response.body#/" <> fieldName
                        }
                    | (fieldName, paramName) <- matches
                    ]
                , olDescription = Just "Inferred: schema field matches path param"
                , olLinkName = Nothing
                }
            | not (null matches)
            ]

-- | Get path parameters from an operation.
getPathParams :: ResolvedOperation -> [Text]
getPathParams op = [rpName p | p <- roParameters op, rpLocation p == ParamPath]

{- | Match response field names to path parameter names.

Uses flexible matching to handle naming conventions:

* Exact match: @id@ -> @id@
* Case normalization: @userId@ -> @user_id@
* Suffix matching: @resourceId@ -> @id@ (when resource matches)

>>> matchResponseFieldsToParams ["id", "name"] ["id"]
[("id", "id")]

>>> matchResponseFieldsToParams ["userId"] ["user_id"]
[("userId", "user_id")]
-}
matchResponseFieldsToParams :: [Text] -> [Text] -> [(Text, Text)]
matchResponseFieldsToParams fields params =
    [(f, p) | f <- fields, p <- params, namesMatch f p]

{- | Check if a response field name matches a parameter name.

Handles common naming convention differences:

* Exact match
* camelCase to snake_case: @userId@ -> @user_id@
* Different suffixes: @userId@ -> @id@ (prefix stripped)
* Case-insensitive: @ID@ -> @id@

>>> namesMatch "id" "id"
True

>>> namesMatch "userId" "user_id"
True

>>> namesMatch "userId" "userId"
True

>>> namesMatch "id" "name"
False
-}
namesMatch :: Text -> Text -> Bool
namesMatch field param =
    let normalField = normalizeParamName field
        normalParam = normalizeParamName param
     in normalField == normalParam
            || field == param
            || (T.toLower param == "id" && "id" `T.isSuffixOf` T.toLower field) -- Also match if field ends with param (e.g., userId matches id)

{- | Normalize a parameter name for comparison.

Converts to a canonical form:

* Lowercase
* camelCase -> snake_case
* Remove underscores for comparison

>>> normalizeParamName "userId"
"userid"

>>> normalizeParamName "user_id"
"userid"

>>> normalizeParamName "UserID"
"userid"
-}
normalizeParamName :: Text -> Text
normalizeParamName name =
    T.toLower $ T.filter (/= '_') $ camelToSnake name
  where
    camelToSnake :: Text -> Text
    camelToSnake = T.pack . go . T.unpack
      where
        go [] = []
        go (c : cs)
            | isUpper c = '_' : toLower c : go cs
            | otherwise = c : go cs

-- | Get a label for an operation (operationId or "METHOD path").
operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op
