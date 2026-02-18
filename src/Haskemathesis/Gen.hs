{- | Public generator entry points and default format registry.

This module provides the main interface for generating JSON values
from JSON Schema definitions. It re-exports the core generation functions
and types needed for custom generators.

=== Basic Usage

Generate a random JSON value from a schema:

@
import Haskemathesis.Gen (genFromSchema)
import Haskemathesis.Schema (emptySchema, schemaType, SString)

mySchema :: Schema
mySchema = emptySchema { schemaType = Just SString }

-- In a Hedgehog property
value <- forAll (genFromSchema mySchema)
@

=== Custom Format Registry

For schemas with custom format annotations, you can provide a custom
'FormatRegistry':

@
import Haskemathesis.Gen.Format (FormatRegistry)
import Haskemathesis.Gen (genFromSchema, defaultFormatRegistry)

myRegistry :: FormatRegistry
myRegistry = defaultFormatRegistry  -- or custom registry
@
-}
module Haskemathesis.Gen (
    FormatRegistry,
    defaultFormatRegistry,
    genFromSchema,
    genFromSchemaWithDepth,
)
where

import Haskemathesis.Gen.Core (genFromSchema, genFromSchemaWithDepth)
import Haskemathesis.Gen.Format (FormatRegistry, defaultFormatRegistry)
