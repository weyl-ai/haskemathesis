module Haskemathesis.Test.Spec (spec) where

import qualified Haskemathesis.Test.Properties.Auth as Auth
import qualified Haskemathesis.Test.Properties.CLI as CLI
import qualified Haskemathesis.Test.Properties.CLIInternal as CLIInternal
import qualified Haskemathesis.Test.Properties.Config as Config
import qualified Haskemathesis.Test.Properties.ContentType as ContentType
import qualified Haskemathesis.Test.Properties.Convert as Convert
import qualified Haskemathesis.Test.Properties.Curl as Curl
import qualified Haskemathesis.Test.Properties.Format as Format
import qualified Haskemathesis.Test.Properties.Gen as Gen
import qualified Haskemathesis.Test.Properties.Helpers as Helpers
import qualified Haskemathesis.Test.Properties.JUnit as JUnit
import qualified Haskemathesis.Test.Properties.Loader as Loader
import qualified Haskemathesis.Test.Properties.Negative as Negative
import qualified Haskemathesis.Test.Properties.Pattern as Pattern
import qualified Haskemathesis.Test.Properties.Primitive as Primitive
import qualified Haskemathesis.Test.Properties.Render as Render
import qualified Haskemathesis.Test.Properties.Request as Request
import qualified Haskemathesis.Test.Properties.Resolve as Resolve
import qualified Haskemathesis.Test.Properties.ResponseHeaders as ResponseHeaders
import qualified Haskemathesis.Test.Properties.ResponseSchema as ResponseSchema
import qualified Haskemathesis.Test.Properties.ResponseTime as ResponseTime
import qualified Haskemathesis.Test.Properties.Stateful.Checks as StatefulChecks
import qualified Haskemathesis.Test.Properties.Stateful.Extract as StatefulExtract
import qualified Haskemathesis.Test.Properties.Stateful.Generator as StatefulGenerator
import qualified Haskemathesis.Test.Properties.Stateful.Heuristics as StatefulHeuristics
import qualified Haskemathesis.Test.Properties.Stateful.Links as StatefulLinks
import qualified Haskemathesis.Test.Properties.Stateful.Sequence as StatefulSequence
import qualified Haskemathesis.Test.Properties.StatusCode as StatusCode
import qualified Haskemathesis.Test.Properties.Streaming as Streaming
import qualified Haskemathesis.Test.Properties.Timeout as Timeout
import qualified Haskemathesis.Test.Properties.Validate as Validate
import qualified Haskemathesis.Test.Properties.Wai as Wai
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
    describe "Haskemathesis" $ do
        Gen.spec
        Primitive.spec
        Pattern.spec
        Format.spec
        JUnit.spec
        Request.spec
        Curl.spec
        Auth.spec
        Config.spec
        CLI.spec
        CLIInternal.spec
        Loader.spec
        Convert.spec
        Negative.spec
        ContentType.spec
        Helpers.spec
        ResponseHeaders.spec
        ResponseTime.spec
        Timeout.spec
        StatusCode.spec
        ResponseSchema.spec
        Streaming.spec
        StatefulExtract.tests
        StatefulGenerator.tests
        StatefulHeuristics.tests
        StatefulLinks.tests
        StatefulSequence.tests
        StatefulChecks.tests
        Validate.spec
        Render.spec
        Resolve.spec
        Wai.spec
