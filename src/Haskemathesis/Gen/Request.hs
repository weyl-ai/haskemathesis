{-# LANGUAGE OverloadedStrings #-}

module Haskemathesis.Gen.Request (
    genApiRequest,
    genParam,
    genRequestBody,
) where

import Data.Aeson (Value (..), encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hedgehog (Gen)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.URI (urlEncode)

import Haskemathesis.Execute.Types
import Haskemathesis.Gen (genFromSchema)
import Haskemathesis.OpenApi.Types

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

genParam :: ResolvedParam -> Gen (ParamLocation, (Text, Text))
genParam param = do
    value <- genFromSchema (rpSchema param)
    pure (rpLocation param, (rpName param, renderValue value))

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
