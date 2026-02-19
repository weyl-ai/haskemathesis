{-# LANGUAGE OverloadedStrings #-}

{- | Operation sequence generation for stateful API testing.

This module generates valid sequences of API operations that can be
executed together, with proper data flow between operations. It uses
operation links to determine which operations can follow others.

=== Key Concepts

* __Sequence Generation__: Create ordered lists of operations where
  each operation can use data from previous operations' responses
* __CRUD Sequences__: Common patterns like Create→Read→Update→Delete
* __Cleanup Steps__: Operations to run after the main test, even on failure

=== Example

@
import Haskemathesis.Stateful.Sequence
import Haskemathesis.Stateful.Heuristics (inferLinks)

-- Generate a random valid sequence
let ops = resolveOperations spec
    links = inferLinks ops
sequence <- Gen.sample $ genOperationSequence 5 ops links
-- sequence might be: POST /users -> GET /users/{id} -> DELETE /users/{id}
@
-}
module Haskemathesis.Stateful.Sequence (
    -- * Sequence Generation
    genOperationSequence,
    genCrudSequence,

    -- * Sequence Building
    buildSequenceFromLinks,
    findLinksFrom,
    findOperationByLabel,

    -- * Step Creation
    createStepFromLink,
    createInitialStep,

    -- * Cleanup
    findCleanupOperations,
    createCleanupSteps,

    -- * Utilities
    canStartSequence,
    getCreatorOperations,
    getResourceOperations,
) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
 )
import Haskemathesis.Stateful.Types (
    OperationLink (..),
    OperationSequence (..),
    ParameterBinding (..),
    SequenceStep (..),
    ValueSource (..),
 )

{- | Generate a random valid operation sequence.

This generator creates sequences where:

1. The first operation is a "creator" (typically POST) or has no required bindings
2. Subsequent operations are linked from previous ones
3. Each step includes proper parameter bindings

=== Parameters

* @maxLength@ - Maximum number of steps in the sequence
* @ops@ - Available operations
* @links@ - Known links between operations

=== Return Value

Returns a 'Gen OperationSequence' that generates valid sequences.

=== Example

@
sequence <- Gen.sample $ genOperationSequence 5 ops links
-- Might generate: POST /users -> GET /users/{id} -> PUT /users/{id}
@
-}
genOperationSequence ::
    Int ->
    [ResolvedOperation] ->
    [OperationLink] ->
    Gen OperationSequence
genOperationSequence maxLength ops links = do
    -- Start with a creator operation or one with no required params
    let starters = filter (canStartSequence ops links) ops
    if null starters
        then pure emptySequence
        else do
            startOp <- Gen.element starters
            let startStep = createInitialStep startOp
                startLabel = operationLabel startOp
            -- Build out the sequence by following links
            numAdditional <- Gen.int (Range.linear 0 (maxLength - 1))
            additionalSteps <- buildSequenceFromLinks numAdditional startLabel ops links
            let allSteps = startStep : additionalSteps
                cleanup = createCleanupSteps ops links allSteps
            pure
                OperationSequence
                    { osSteps = allSteps
                    , osCleanup = cleanup
                    }

{- | Generate a CRUD sequence for a resource.

Creates a sequence that exercises the full lifecycle:
Create → Read → Update → Delete

=== Parameters

* @createOp@ - The POST operation that creates the resource
* @ops@ - All available operations (to find related GET/PUT/DELETE)

=== Return Value

Returns a 'Gen OperationSequence' with CRUD operations.

=== Example

@
let createOp = findOperation "POST" "/users" ops
sequence <- Gen.sample $ genCrudSequence createOp ops
-- Generates: POST /users -> GET /users/{id} -> PUT /users/{id} -> DELETE /users/{id}
@
-}
genCrudSequence ::
    ResolvedOperation ->
    [ResolvedOperation] ->
    Gen OperationSequence
genCrudSequence createOp ops = do
    let createLabel = operationLabel createOp
        createPath = roPath createOp
        -- Find related operations on the resource path
        relatedOps = getResourceOperations createPath ops
        getOps = filter ((== "GET") . roMethod) relatedOps
        putOps = filter ((== "PUT") . roMethod) relatedOps
        patchOps = filter ((== "PATCH") . roMethod) relatedOps
        deleteOps = filter ((== "DELETE") . roMethod) relatedOps

    -- Build the sequence
    let createStep = createInitialStep createOp

    -- Add a GET step if available
    getSteps <- case getOps of
        (getOp : _) -> pure [createResourceStep createLabel getOp]
        [] -> pure []

    -- Optionally add PUT or PATCH
    updateSteps <-
        if null putOps && null patchOps
            then pure []
            else do
                -- Choose PUT or PATCH randomly if both available
                let updateOps = putOps ++ patchOps
                if null updateOps
                    then pure []
                    else do
                        updateOp <- Gen.element updateOps
                        pure [createResourceStep createLabel updateOp]

    -- Add DELETE for cleanup (in main steps, not cleanup)
    deleteSteps <- case deleteOps of
        (deleteOp : _) -> pure [createResourceStep createLabel deleteOp]
        [] -> pure []

    let allSteps = createStep : getSteps ++ updateSteps ++ deleteSteps

    pure
        OperationSequence
            { osSteps = allSteps
            , osCleanup = [] -- DELETE is already in steps
            }

{- | Build additional steps by following links from a starting operation.

Recursively follows links to build a sequence of operations.
-}
buildSequenceFromLinks ::
    Int ->
    Text ->
    [ResolvedOperation] ->
    [OperationLink] ->
    Gen [SequenceStep]
buildSequenceFromLinks 0 _ _ _ = pure []
buildSequenceFromLinks remaining currentLabel ops links = do
    let availableLinks = findLinksFrom currentLabel links
    if null availableLinks
        then pure []
        else do
            -- Pick a random link to follow
            link <- Gen.element availableLinks
            let step = createStepFromLink link
                nextLabel = olTargetOperation link
            -- Continue building
            moreSteps <- buildSequenceFromLinks (remaining - 1) nextLabel ops links
            pure (step : moreSteps)

-- | Find all links that start from a given operation.
findLinksFrom :: Text -> [OperationLink] -> [OperationLink]
findLinksFrom opLabel = filter ((== opLabel) . olSourceOperation)

-- | Find an operation by its label (operationId or "METHOD path").
findOperationByLabel :: Text -> [ResolvedOperation] -> Maybe ResolvedOperation
findOperationByLabel label = go
  where
    go [] = Nothing
    go (op : rest)
        | operationLabel op == label = Just op
        | otherwise = go rest

{- | Create a sequence step from an operation link.

Converts the link's parameter bindings to the step format.
-}
createStepFromLink :: OperationLink -> SequenceStep
createStepFromLink link =
    SequenceStep
        { ssOperationId = olTargetOperation link
        , ssParamBindings = bindingsMap
        }
  where
    bindingsMap =
        Map.fromList
            [(pbTargetParam pb, pbSource pb) | pb <- olParameterBindings link]

-- | Create an initial step (no bindings, all params generated randomly).
createInitialStep :: ResolvedOperation -> SequenceStep
createInitialStep op =
    SequenceStep
        { ssOperationId = operationLabel op
        , ssParamBindings = Map.empty
        }

-- | Create a step that uses extracted values from a previous create operation.
createResourceStep :: Text -> ResolvedOperation -> SequenceStep
createResourceStep _sourceLabel op =
    SequenceStep
        { ssOperationId = operationLabel op
        , ssParamBindings = pathParamBindings
        }
  where
    -- Bind all path params from state (assuming they were extracted)
    pathParamBindings =
        Map.fromList
            [ (rpName p, FromState (rpName p))
            | p <- roParameters op
            , rpLocation p == ParamPath
            ]

-- | Find DELETE operations that can clean up resources created in the sequence.
findCleanupOperations ::
    [ResolvedOperation] ->
    [OperationLink] ->
    [SequenceStep] ->
    [ResolvedOperation]
findCleanupOperations ops links steps =
    let
        -- Find all creator operations in the sequence
        creatorLabels = [ssOperationId s | s <- steps, isCreatorStep s ops]
        -- Find DELETE links from those creators
        deleteLinks =
            [ link
            | link <- links
            , olSourceOperation link `elem` creatorLabels
            , isDeleteOperation (olTargetOperation link) ops
            ]
        -- Get the target operations
        deleteOpLabels = map olTargetOperation deleteLinks
     in
        mapMaybe (`findOperationByLabel` ops) deleteOpLabels

-- | Create cleanup steps for operations.
createCleanupSteps ::
    [ResolvedOperation] ->
    [OperationLink] ->
    [SequenceStep] ->
    [SequenceStep]
createCleanupSteps ops links steps =
    let deleteOps = findCleanupOperations ops links steps
        -- Check if DELETE is already in the main steps
        mainDeleteLabels = [ssOperationId s | s <- steps, isDeleteStep s ops]
     in [ createResourceStep "" op
        | op <- deleteOps
        , operationLabel op `notElem` mainDeleteLabels
        ]

{- | Check if an operation can start a sequence.

An operation can start a sequence if:
1. It has no required path parameters, OR
2. It's a POST (creator) operation
-}
canStartSequence ::
    [ResolvedOperation] ->
    [OperationLink] ->
    ResolvedOperation ->
    Bool
canStartSequence _ops _links op =
    -- Can start if it's a creator or has no required path params
    isCreatorOperation op || not (hasRequiredPathParams op)

-- | Get all creator operations (typically POSTs to collection endpoints).
getCreatorOperations :: [ResolvedOperation] -> [ResolvedOperation]
getCreatorOperations = filter isCreatorOperation

{- | Get operations related to a resource path.

Given a collection path like "/users", finds operations on "/users/{id}".
-}
getResourceOperations :: Text -> [ResolvedOperation] -> [ResolvedOperation]
getResourceOperations basePath ops =
    [ op
    | op <- ops
    , let path = roPath op
    , path /= basePath -- Different from base
    , basePath `T.isPrefixOf` path -- Starts with base
    , hasPathParams op -- Has path parameters
    ]

-- Internal helpers

emptySequence :: OperationSequence
emptySequence = OperationSequence [] []

operationLabel :: ResolvedOperation -> Text
operationLabel op =
    case roOperationId op of
        Just opId -> opId
        Nothing -> roMethod op <> " " <> roPath op

isCreatorOperation :: ResolvedOperation -> Bool
isCreatorOperation op = roMethod op == "POST"

hasRequiredPathParams :: ResolvedOperation -> Bool
hasRequiredPathParams op =
    any isRequiredPathParam (roParameters op)
  where
    isRequiredPathParam p = rpLocation p == ParamPath && rpRequired p

hasPathParams :: ResolvedOperation -> Bool
hasPathParams op = any ((== ParamPath) . rpLocation) (roParameters op)

isCreatorStep :: SequenceStep -> [ResolvedOperation] -> Bool
isCreatorStep step ops =
    maybe False isCreatorOperation (findOperationByLabel (ssOperationId step) ops)

isDeleteOperation :: Text -> [ResolvedOperation] -> Bool
isDeleteOperation label ops =
    case findOperationByLabel label ops of
        Just op -> roMethod op == "DELETE"
        Nothing -> False

isDeleteStep :: SequenceStep -> [ResolvedOperation] -> Bool
isDeleteStep step ops =
    case findOperationByLabel (ssOperationId step) ops of
        Just op -> roMethod op == "DELETE"
        Nothing -> False
