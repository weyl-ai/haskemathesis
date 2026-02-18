{-# LANGUAGE OverloadedStrings #-}

{- | Generate API requests from resolved operations.

This module provides generators for creating 'ApiRequest' values from
'ResolvedOperation' definitions. It handles parameter generation
(path, query, header), request body generation, and path interpolation.

=== Basic Usage

@
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation)

-- Generate a request for an operation
let operation :: ResolvedOperation = ...
request <- genApiRequest operation
@
-}
module Haskemathesis.Gen.Request (
    genApiRequest,
    genParam,
    genRequestBody,
)
where

import Data.Aeson (Value (..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Haskemathesis.Execute.Types
import Haskemathesis.Gen (genFromSchema)
import Haskemathesis.OpenApi.Types
import Hedgehog (Gen)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.URI (urlEncode)

{- | Generate a complete API request for a resolved operation.

This generator creates an 'ApiRequest' by:

1. Generating values for all parameters (path, query, header)
2. Generating a request body if the operation has one
3. Interpolating path parameters into the URL path

=== Parameters

* @op@ - The 'ResolvedOperation' to generate a request for

=== Return Value

Returns a 'Gen ApiRequest' that generates valid requests for the operation.

=== Example

@
import Haskemathesis.Gen.Request (genApiRequest)
import Haskemathesis.OpenApi.Types (ResolvedOperation)

let operation :: ResolvedOperation = ...
request <- genApiRequest operation
-- request will have all required parameters and body filled in
@
-}
genApiRequest :: ResolvedOperation -> Gen ApiRequest
genApiRequest op = do
    params <- traverse genParam (roParameters op)
    body <- traverse genRequestBody (roRequestBody op)
    let (pathParams, queryParams, headerParams) = partitionParams params
    let path = interpolatePath (roPath op) pathParams
    pure
        ApiRequest
            { reqMethod = encodeUtf8 (roMethod op)
            , reqPath = path
            , reqQueryParams = queryParams
            , reqHeaders = headerParams
            , reqBody = body
            }

{- | Generate a parameter value.

This generator creates a value for a single parameter based on its
schema. The generated value is returned along with its location and name
so it can be properly placed in the request (path, query, or header).

=== Parameters

* @param@ - The 'ResolvedParam' to generate a value for

=== Return Value

Returns a 'Gen' that produces a tuple of (location, (name, value)).

=== Example

@
import Haskemathesis.Gen.Request (genParam)
import Haskemathesis.OpenApi.Types (ResolvedParam(..), ParamLocation(..))

let param = ResolvedParam
        { rpName = "userId"
        , rpLocation = ParamPath
        , rpRequired = True
        , rpSchema = ...
        }
(location, (name, value)) <- genParam param
@
-}
genParam :: ResolvedParam -> Gen (ParamLocation, (Text, Text))
genParam param = do
    value <- genFromSchema (rpSchema param)
    pure (rpLocation param, (rpName param, renderValue value))

{- | Generate a request body.

This generator creates a request body by generating a value according
to the body schema and encoding it as JSON. The content type is preserved
from the resolved request body definition.

=== Parameters

* @body@ - The 'ResolvedRequestBody' to generate content for

=== Return Value

Returns a 'Gen' that produces a tuple of (contentType, bodyBytes).

=== Example

@
import Haskemathesis.Gen.Request (genRequestBody)
import Haskemathesis.OpenApi.Types (ResolvedRequestBody(..))

let bodyDef = ResolvedRequestBody
        { rbContentType = "application/json"
        , rbSchema = ...
        }
(contentType, bodyBytes) <- genRequestBody bodyDef
@
-}
genRequestBody :: ResolvedRequestBody -> Gen (MediaType, BS.ByteString)
genRequestBody body = do
    value <- genFromSchema (rbSchema body)
    pure (rbContentType body, LBS.toStrict (encode value))

partitionParams ::
    [(ParamLocation, (Text, Text))] ->
    ([(Text, Text)], [(Text, Text)], [(HeaderName, BS.ByteString)])
partitionParams =
    foldr step ([], [], [])
  where
    step (location, (name, value)) (paths, queries, headers) =
        case location of
            ParamPath -> ((name, value) : paths, queries, headers)
            ParamQuery -> (paths, (name, value) : queries, headers)
            ParamHeader -> (paths, queries, toHeader name value : headers)
            ParamCookie -> (paths, queries, headers)

toHeader :: Text -> Text -> (HeaderName, BS.ByteString)
toHeader name value = (CI.mk (encodeUtf8 name), encodeUtf8 value)

interpolatePath :: Text -> [(Text, Text)] -> Text
interpolatePath =
    foldl' replace
  where
    replace acc (name, value) =
        T.replace ("{" <> name <> "}") (encodePathSegment value) acc

encodePathSegment :: Text -> Text
encodePathSegment = decodeUtf8 . urlEncode False . encodeUtf8

renderValue :: Value -> Text
renderValue value =
    case value of
        String txt -> txt
        Number n -> T.pack (show n)
        Bool True -> "true"
        Bool False -> "false"
        Null -> "null"
        Array _ -> jsonText
        Object _ -> jsonText
  where
    jsonText = decodeUtf8 (LBS.toStrict (encode value))
