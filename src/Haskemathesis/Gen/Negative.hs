-- | Negative request generation for invalid inputs.
module Haskemathesis.Gen.Negative (
    NegativeMutation (..),
    renderNegativeMutation,
    genNegativeRequest,
    applyNegativeMutation,
) where

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

import Haskemathesis.Execute.Types (ApiRequest (..))
import Haskemathesis.Gen.Negative.Mutations (applyNegativeMutation, mutationCandidates)
import Haskemathesis.Gen.Negative.Types (NegativeMutation (..), renderNegativeMutation)
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation (..))

{- | Generate a negative request by applying a single mutation.
Returns Nothing when no applicable mutation exists for the operation.
-}
genNegativeRequest :: ResolvedOperation -> Gen (Maybe (ApiRequest, NegativeMutation))
genNegativeRequest op = do
    req <- genApiRequest op
    let candidates = mutationCandidates op req
    case candidates of
        [] -> pure Nothing
        _ -> do
            (mutation, apply) <- Gen.element candidates
            pure (Just (apply req, mutation))
