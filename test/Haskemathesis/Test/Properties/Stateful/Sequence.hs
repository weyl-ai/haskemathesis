{-# LANGUAGE OverloadedStrings #-}

-- | Property tests for operation sequence generation.
module Haskemathesis.Test.Properties.Stateful.Sequence (
    tests,
) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Hedgehog (Property, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe)

import Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedOperation (..),
    ResolvedParam (..),
 )
import Haskemathesis.Schema (emptySchema)
import Haskemathesis.Stateful.Sequence
import Haskemathesis.Stateful.Types (
    OperationLink (..),
    OperationSequence (..),
    ParameterBinding (..),
    SequenceStep (..),
    ValueSource (..),
 )
import Haskemathesis.Test.Support (itProp)

tests :: Spec
tests = describe "Stateful.Sequence" $ do
    describe "findLinksFrom" $ do
        itProp "finds links from source operation" propFindLinksFrom
        itProp "returns empty for unknown source" propFindLinksFromEmpty
        itProp "finds multiple links from same source" propFindLinksFromMultiple

    describe "findOperationByLabel" $ do
        itProp "finds by operationId" propFindByOperationId
        itProp "finds by method+path label" propFindByMethodPath
        itProp "returns Nothing for unknown label" propFindByLabelNotFound

    describe "createStepFromLink" $ do
        itProp "creates step with correct target" propCreateStepTarget
        itProp "creates step with bindings" propCreateStepBindings

    describe "createInitialStep" $ do
        itProp "creates step with empty bindings" propCreateInitialEmpty
        itProp "uses operationId as label" propCreateInitialLabel

    describe "canStartSequence" $ do
        itProp "POST can start sequence" propCanStartPost
        itProp "GET with required path params cannot start" propCannotStartGetWithParams
        itProp "GET without params can start" propCanStartGetNoParams

    describe "getCreatorOperations" $ do
        itProp "returns POST operations" propGetCreatorsPosts
        itProp "excludes GET operations" propGetCreatorsExcludesGet

    describe "getResourceOperations" $ do
        itProp "finds operations on resource path" propGetResourceOps
        itProp "excludes collection operation" propGetResourceOpsExcludesBase

    describe "genOperationSequence" $ do
        itProp "generates non-empty sequence from creators" propGenSeqNonEmpty
        itProp "respects max length" propGenSeqMaxLength

    describe "genCrudSequence" $ do
        itProp "starts with create operation" propCrudStartsWithCreate
        itProp "includes at least create step" propCrudHasCreate

-- findLinksFrom properties

propFindLinksFrom :: Property
propFindLinksFrom = property $ do
    let link1 = mkLink "createUser" "getUser"
        link2 = mkLink "createUser" "deleteUser"
        link3 = mkLink "getUser" "updateUser"
        links = [link1, link2, link3]
        result = findLinksFrom "createUser" links
    length result === 2

propFindLinksFromEmpty :: Property
propFindLinksFromEmpty = property $ do
    let link = mkLink "createUser" "getUser"
        result = findLinksFrom "unknownOp" [link]
    result === []

propFindLinksFromMultiple :: Property
propFindLinksFromMultiple = property $ do
    let links =
            [ mkLink "createUser" "getUser"
            , mkLink "createUser" "updateUser"
            , mkLink "createUser" "deleteUser"
            ]
        result = findLinksFrom "createUser" links
    length result === 3

-- findOperationByLabel properties

propFindByOperationId :: Property
propFindByOperationId = property $ do
    let op = mkOp "POST" "/users" (Just "createUser") []
        result = findOperationByLabel "createUser" [op]
    case result of
        Just found -> roOperationId found === Just "createUser"
        Nothing -> assert False

propFindByMethodPath :: Property
propFindByMethodPath = property $ do
    let op = mkOp "GET" "/users" Nothing []
        result = findOperationByLabel "GET /users" [op]
    case result of
        Just found -> roPath found === "/users"
        Nothing -> assert False

propFindByLabelNotFound :: Property
propFindByLabelNotFound = property $ do
    let op = mkOp "POST" "/users" (Just "createUser") []
        result = findOperationByLabel "unknownOp" [op]
    result === Nothing

-- createStepFromLink properties

propCreateStepTarget :: Property
propCreateStepTarget = property $ do
    let link = mkLink "createUser" "getUser"
        step = createStepFromLink link
    ssOperationId step === "getUser"

propCreateStepBindings :: Property
propCreateStepBindings = property $ do
    let link = mkLinkWithBindings "createUser" "getUser" [("id", FromState "id")]
        step = createStepFromLink link
    Map.lookup "id" (ssParamBindings step) === Just (FromState "id")

-- createInitialStep properties

propCreateInitialEmpty :: Property
propCreateInitialEmpty = property $ do
    let op = mkOp "POST" "/users" (Just "createUser") []
        step = createInitialStep op
    ssParamBindings step === Map.empty

propCreateInitialLabel :: Property
propCreateInitialLabel = property $ do
    let op = mkOp "POST" "/users" (Just "createUser") []
        step = createInitialStep op
    ssOperationId step === "createUser"

-- canStartSequence properties

propCanStartPost :: Property
propCanStartPost = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
    assert $ canStartSequence [] [] postOp

propCannotStartGetWithParams :: Property
propCannotStartGetWithParams = property $ do
    let getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
    assert $ not $ canStartSequence [] [] getOp

propCanStartGetNoParams :: Property
propCanStartGetNoParams = property $ do
    let getOp = mkOp "GET" "/users" (Just "listUsers") []
    assert $ canStartSequence [] [] getOp

-- getCreatorOperations properties

propGetCreatorsPosts :: Property
propGetCreatorsPosts = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users" (Just "listUsers") []
        creators = getCreatorOperations [postOp, getOp]
    length creators === 1
    case creators of
        [op] -> roMethod op === "POST"
        _ -> assert False

propGetCreatorsExcludesGet :: Property
propGetCreatorsExcludesGet = property $ do
    let getOp = mkOp "GET" "/users" (Just "listUsers") []
        creators = getCreatorOperations [getOp]
    creators === []

-- getResourceOperations properties

propGetResourceOps :: Property
propGetResourceOps = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        deleteOp = mkOp "DELETE" "/users/{id}" (Just "deleteUser") [mkPathParam "id"]
        ops = [postOp, getOp, deleteOp]
        resources = getResourceOperations "/users" ops
    length resources === 2

propGetResourceOpsExcludesBase :: Property
propGetResourceOpsExcludesBase = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        ops = [postOp, getOp]
        resources = getResourceOperations "/users" ops
    -- Should not include the POST /users
    assert $ all ((/= "/users") . roPath) resources

-- genOperationSequence properties

propGenSeqNonEmpty :: Property
propGenSeqNonEmpty = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        links = [mkLink "createUser" "getUser"]
    seq' <- forAll $ genOperationSequence 5 [postOp, getOp] links
    assert $ not $ null $ osSteps seq'

propGenSeqMaxLength :: Property
propGenSeqMaxLength = property $ do
    maxLen <- forAll $ Gen.int (Range.linear 1 10)
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        links = [mkLink "createUser" "getUser"]
    seq' <- forAll $ genOperationSequence maxLen [postOp, getOp] links
    assert $ length (osSteps seq') <= maxLen

-- genCrudSequence properties

propCrudStartsWithCreate :: Property
propCrudStartsWithCreate = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
        getOp = mkOp "GET" "/users/{id}" (Just "getUser") [mkPathParam "id"]
        deleteOp = mkOp "DELETE" "/users/{id}" (Just "deleteUser") [mkPathParam "id"]
        ops = [postOp, getOp, deleteOp]
    seq' <- forAll $ genCrudSequence postOp ops
    case osSteps seq' of
        (first : _) -> ssOperationId first === "createUser"
        [] -> assert False

propCrudHasCreate :: Property
propCrudHasCreate = property $ do
    let postOp = mkOp "POST" "/users" (Just "createUser") []
    seq' <- forAll $ genCrudSequence postOp [postOp]
    assert $ not $ null $ osSteps seq'

-- Helper functions

mkOp :: Text -> Text -> Maybe Text -> [ResolvedParam] -> ResolvedOperation
mkOp method path opId params =
    ResolvedOperation
        { roMethod = method
        , roPath = path
        , roOperationId = opId
        , roTags = []
        , roParameters = params
        , roRequestBody = Nothing
        , roResponses = Map.empty
        , roDefaultResponse = Nothing
        , roSecurity = []
        , roIsStreaming = False
        , roTimeout = Nothing
        }

mkPathParam :: Text -> ResolvedParam
mkPathParam name =
    ResolvedParam
        { rpName = name
        , rpLocation = ParamPath
        , rpRequired = True
        , rpSchema = emptySchema
        }

mkLink :: Text -> Text -> OperationLink
mkLink source target =
    OperationLink
        { olSourceOperation = source
        , olTargetOperation = target
        , olParameterBindings = []
        , olDescription = Nothing
        , olLinkName = Nothing
        }

mkLinkWithBindings :: Text -> Text -> [(Text, ValueSource)] -> OperationLink
mkLinkWithBindings source target bindings =
    OperationLink
        { olSourceOperation = source
        , olTargetOperation = target
        , olParameterBindings = [ParameterBinding p s | (p, s) <- bindings]
        , olDescription = Nothing
        , olLinkName = Nothing
        }
