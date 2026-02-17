{-# LANGUAGE StrictData #-}

module Haskemathesis.Execute.Types (
    ApiRequest (..),
    ApiResponse (..),
    MediaType,
    BaseUrl,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.HTTP.Types (HeaderName, Method)

type MediaType = Text
type BaseUrl = Text

data ApiRequest = ApiRequest
    { reqMethod :: Method
    , reqPath :: Text
    , reqQueryParams :: [(Text, Text)]
    , reqHeaders :: [(HeaderName, ByteString)]
    , reqBody :: Maybe (MediaType, ByteString)
    }
    deriving (Eq, Show)

data ApiResponse = ApiResponse
    { resStatusCode :: Int
    , resHeaders :: [(HeaderName, ByteString)]
    , resBody :: ByteString
    , resTime :: NominalDiffTime
    }
    deriving (Eq, Show)
