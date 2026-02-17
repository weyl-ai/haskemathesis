module Haskemathesis.Gen.Primitive (
    genString,
    genInteger,
    genNumber,
    genBoolean,
) where

import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Regex.TDFA ((=~))

import Haskemathesis.Schema

genString :: Schema -> Gen Value
genString schema =
    case schemaEnum schema of
        Just xs | not (null xs) -> Gen.element xs
        _ ->
            case schemaConst schema of
                Just v -> pure v
                Nothing ->
                    case schemaPattern schema of
                        Just pat ->
                            let minL = fromMaybe 0 (schemaMinLength schema)
                                maxL = fromMaybe (minL + 8) (schemaMaxLength schema)
                                range = Range.linear minL maxL
                             in Gen.filter
                                    (matchesPattern pat)
                                    (String <$> Gen.text range Gen.alphaNum)
                        Nothing -> do
                            let minL = fromMaybe 0 (schemaMinLength schema)
                                maxL = fromMaybe (minL + 32) (schemaMaxLength schema)
                            String <$> Gen.text (Range.linear minL maxL) Gen.alphaNum

genInteger :: Schema -> Gen Value
genInteger schema =
    Number . fromIntegral <$> Gen.int (Range.linearFrom 0 lo hi)
  where
    lo :: Int
    lo =
        max
            (maybe (-1000) ceiling (schemaMinimum schema))
            (maybe (-1000) ((+ 1) . floor) (schemaExclusiveMinimum schema))
    hi :: Int
    hi =
        min
            (maybe 1000 floor (schemaMaximum schema))
            (maybe 1000 (subtract 1 . ceiling) (schemaExclusiveMaximum schema))

genNumber :: Schema -> Gen Value
genNumber schema =
    Number . fromFloatDigits
        <$> Gen.double (Range.linearFracFrom 0 lo hi)
  where
    lo =
        case schemaExclusiveMinimum schema of
            Just minV -> minV + 1e-6
            Nothing -> fromMaybe (-1000) (schemaMinimum schema)
    hi =
        case schemaExclusiveMaximum schema of
            Just maxV -> maxV - 1e-6
            Nothing -> fromMaybe 1000 (schemaMaximum schema)

genBoolean :: Schema -> Gen Value
genBoolean _ = Bool <$> Gen.bool

matchesPattern :: Text -> Value -> Bool
matchesPattern pat value =
    case value of
        String txt -> T.unpack txt =~ T.unpack pat
        _ -> False
