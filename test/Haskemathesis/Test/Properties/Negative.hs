module Haskemathesis.Test.Properties.Negative (spec) where

import Test.Hspec (Spec)

import qualified Haskemathesis.Test.Properties.Negative.Checks as Checks
import qualified Haskemathesis.Test.Properties.Negative.Generation as Generation
import qualified Haskemathesis.Test.Properties.Negative.Mutations as Mutations

spec :: Spec
spec = do
    Generation.spec
    Mutations.spec
    Checks.spec
