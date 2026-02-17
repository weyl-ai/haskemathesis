{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Gen.Negative.Types (
    NegativeMutation (..),
    renderNegativeMutation,
) where

import Data.Text (Text)

-- | Mutations applied to a valid request to create a negative test case.
data NegativeMutation
    = RemoveRequiredPath !Text
    | RemoveRequiredHeader !Text
    | RemoveRequiredQuery !Text
    | InvalidPathParam !Text
    | InvalidHeader !Text
    | InvalidQueryParam !Text
    | InvalidRequestBody
    | InvalidContentType
    deriving (Eq, Show)

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
