{- | CLI entry point for Haskemathesis.

This module provides the main entry point for the Haskemathesis CLI,
allowing users to run property-based API tests from the command line
without writing Haskell code.

== Usage

Run tests against an API:

@
haskemathesis test --spec openapi.yaml --url http://localhost:8080
@

Validate an OpenAPI spec:

@
haskemathesis validate --spec openapi.yaml
@

Generate curl commands:

@
haskemathesis curl --spec openapi.yaml --url http://localhost:8080
@
-}
module Main (main) where

import Haskemathesis.CLI (runCLI)

-- | Main entry point for the CLI.
main :: IO ()
main = runCLI
