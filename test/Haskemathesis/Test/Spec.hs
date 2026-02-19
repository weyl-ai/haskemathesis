module Haskemathesis.Test.Spec (spec) where

import qualified Haskemathesis.Test.Properties.Auth as Auth
import qualified Haskemathesis.Test.Properties.CLI as CLI
import qualified Haskemathesis.Test.Properties.Config as Config
import qualified Haskemathesis.Test.Properties.ContentType as ContentType
import qualified Haskemathesis.Test.Properties.Curl as Curl
import qualified Haskemathesis.Test.Properties.Gen as Gen
import qualified Haskemathesis.Test.Properties.Loader as Loader
import qualified Haskemathesis.Test.Properties.Negative as Negative
import qualified Haskemathesis.Test.Properties.Render as Render
import qualified Haskemathesis.Test.Properties.Request as Request
import qualified Haskemathesis.Test.Properties.Resolve as Resolve
import qualified Haskemathesis.Test.Properties.ResponseHeaders as ResponseHeaders
import qualified Haskemathesis.Test.Properties.ResponseSchema as ResponseSchema
import qualified Haskemathesis.Test.Properties.StatusCode as StatusCode
import qualified Haskemathesis.Test.Properties.Streaming as Streaming
import qualified Haskemathesis.Test.Properties.Validate as Validate
import qualified Haskemathesis.Test.Properties.Wai as Wai
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
    describe "Haskemathesis" $ do
        Gen.spec
        Request.spec
        Curl.spec
        Auth.spec
        Config.spec
        CLI.spec
        Loader.spec
        Negative.spec
        ContentType.spec
        ResponseHeaders.spec
        StatusCode.spec
        ResponseSchema.spec
        Streaming.spec
        Validate.spec
        Render.spec
        Resolve.spec
        Wai.spec
