{-# LANGUAGE OverloadedStrings #-}

{- | Types for negative testing mutations.

This module defines the 'NegativeMutation' type, which represents
various ways to make a valid request invalid. These mutations are
used in negative testing to verify that the API properly rejects
malformed input.

=== Mutation Types

* 'RemoveRequiredPath' - Remove a required path parameter
* 'RemoveRequiredHeader' - Remove a required header
* 'RemoveRequiredQuery' - Remove a required query parameter
* 'InvalidPathParam' - Replace a path parameter with an invalid value
* 'InvalidHeader' - Replace a header with an invalid value
* 'InvalidQueryParam' - Replace a query parameter with an invalid value
* 'InvalidRequestBody' - Replace the request body with an invalid value
* 'InvalidContentType' - Change the Content-Type to an invalid value

=== Basic Usage

@
import Haskemathesis.Gen.Negative.Types (NegativeMutation(..), renderNegativeMutation)

let mutation = RemoveRequiredQuery "page"
    description = renderNegativeMutation mutation
-- description == "remove required query param: page"
@
-}
module Haskemathesis.Gen.Negative.Types (
    NegativeMutation (..),
    renderNegativeMutation,
)
where

import Data.Text (Text)

{- | Mutations applied to a valid request to create a negative test case.

Each constructor represents a different way to make a request invalid.
These mutations are used to test that the API properly rejects
malformed input.
-}
data NegativeMutation
    = -- | Remove a required path parameter (will result in 404 or error).
      RemoveRequiredPath !Text
    | -- | Remove a required header from the request.
      RemoveRequiredHeader !Text
    | -- | Remove a required query parameter from the request.
      RemoveRequiredQuery !Text
    | -- | Replace a path parameter with an invalid value.
      InvalidPathParam !Text
    | -- | Replace a header with an invalid value.
      InvalidHeader !Text
    | -- | Replace a query parameter with an invalid value.
      InvalidQueryParam !Text
    | -- | Replace the request body with an invalid value.
      InvalidRequestBody
    | -- | Change the Content-Type to an invalid value.
      InvalidContentType
    deriving (Eq, Show)

{- | Render a mutation as a human-readable description.

This function converts a 'NegativeMutation' into a descriptive string
suitable for display in test output or failure messages.

=== Parameters

* @mutation@ - The 'NegativeMutation' to describe

=== Return Value

Returns a 'Text' containing the human-readable description.

=== Example

@
renderNegativeMutation (RemoveRequiredQuery "page")
-- Returns: "remove required query param: page"

renderNegativeMutation InvalidRequestBody
-- Returns: "invalid request body"
@
-}
renderNegativeMutation :: NegativeMutation -> Text
renderNegativeMutation mutation =
    case mutation of
        RemoveRequiredPath name -> "remove required path param: " <> name
        RemoveRequiredHeader name -> "remove required header: " <> name
        RemoveRequiredQuery name -> "remove required query param: " <> name
        InvalidPathParam name -> "invalid path param value: " <> name
        InvalidHeader name -> "invalid header value: " <> name
        InvalidQueryParam name -> "invalid query param value: " <> name
        InvalidRequestBody -> "invalid request body"
        InvalidContentType -> "invalid content type"
