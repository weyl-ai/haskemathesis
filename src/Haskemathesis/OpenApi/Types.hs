{-# LANGUAGE StrictData #-}

module Haskemathesis.OpenApi.Types (
    ParamLocation (..),
    ResolvedParam (..),
    ResolvedRequestBody (..),
    ResponseSpec (..),
    ResolvedOperation (..),
) where

import Data.Map.Strict (Map)
import Data.Text (Text)

import Haskemathesis.Schema (Schema)

data ParamLocation
    = ParamPath
    | ParamQuery
    | ParamHeader
    | ParamCookie
    deriving (Eq, Show)

data ResolvedParam = ResolvedParam
    { rpName :: Text
    , rpLocation :: ParamLocation
    , rpRequired :: Bool
    , rpSchema :: Schema
    }
    deriving (Eq, Show)

data ResolvedRequestBody = ResolvedRequestBody
    { rbContentType :: Text
    , rbSchema :: Schema
    }
    deriving (Eq, Show)

data ResponseSpec = ResponseSpec
    { rsContent :: Map Text Schema
    , rsHeaders :: Map Text Schema
    , rsRequiredHeaders :: [Text]
    }
    deriving (Eq, Show)

data ResolvedOperation = ResolvedOperation
    { roMethod :: Text
    , roPath :: Text
    , roOperationId :: Maybe Text
    , roParameters :: [ResolvedParam]
    , roRequestBody :: Maybe ResolvedRequestBody
    , roResponses :: Map Int ResponseSpec
    , roDefaultResponse :: Maybe ResponseSpec
    }
    deriving (Eq, Show)
