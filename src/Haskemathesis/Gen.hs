-- | Public generator entry points and default format registry.
module Haskemathesis.Gen (
    FormatRegistry,
    defaultFormatRegistry,
    genFromSchema,
    genFromSchemaWithDepth,
) where

import Haskemathesis.Gen.Core (genFromSchema, genFromSchemaWithDepth)
import Haskemathesis.Gen.Format (FormatRegistry, defaultFormatRegistry)
