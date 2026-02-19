{- | Pure regex pattern to string generator.

This module generates strings that match a given regular expression pattern,
without requiring external SMT solvers. It uses the LogicT monad for
backtracking enumeration of possible matches.

Adapted from regex-genex (Audrey Tang, Public Domain/CC0).

=== Supported Features

* Character classes: @[abc]@, @[^abc]@, @[a-z]@
* Predefined classes: @\\d@, @\\w@, @\\s@, @\\D@, @\\W@, @\\S@
* Quantifiers: @*@, @+@, @?@, @{n}@, @{n,m}@
* Alternation: @a|b@
* Grouping: @(abc)@
* Escapes: @\\n@, @\\t@, @\\r@, @\\.@ etc.
* Anchors: @^@, @$@ (treated as zero-width, always match)
* Dot: @.@ (matches printable ASCII)

=== Limitations

* No support for backreferences (@\\1@, @\\2@, etc.)
* No support for word boundaries (@\\b@)
* No support for lookahead/lookbehind
* No Unicode character classes
* @*@ and @+@ are bounded to prevent infinite generation

=== Example

@
import Haskemathesis.Gen.Pattern (genFromPattern)
import Hedgehog (Gen)

emailPatternGen :: Gen Text
emailPatternGen = genFromPattern "[a-z]+@[a-z]+\\.[a-z]{2,4}"
@
-}
module Haskemathesis.Gen.Pattern (
    genFromPattern,
    genFromPatternWithLength,
    generateMatchesFromText,
    generateMatches,
    parsePattern,
) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (replicateM)
import Control.Monad.Logic (Logic, observeMany)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Set (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.TDFA.Pattern (
    Pattern (..),
    PatternSet (..),
 )
import Text.Regex.TDFA.ReadRegex (parseRegex)

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

-- | Maximum number of repetitions for unbounded quantifiers (* and +)
maxRepeat :: Int
maxRepeat = 4

{- | Generate a Hedgehog generator for strings matching a regex pattern.

The generator picks uniformly from a set of possible matches.
For unbounded patterns, only a limited set is generated.

Returns 'Nothing' if the pattern cannot be parsed.
-}
genFromPattern :: Text -> Maybe (Gen Text)
genFromPattern patternText =
    case parsePattern (T.unpack patternText) of
        Left _err -> Nothing
        Right pat ->
            let matches = generateMatches pat
             in if null matches
                    then Nothing
                    else Just (Gen.element matches)

{- | Generate a Hedgehog generator for strings matching a regex pattern,
with optional length constraints.

Returns 'Nothing' if the pattern cannot be parsed or no matches satisfy
the length constraints.
-}
genFromPatternWithLength :: Text -> Maybe Int -> Maybe Int -> Maybe (Gen Text)
genFromPatternWithLength patternText minLen maxLen =
    case parsePattern (T.unpack patternText) of
        Left _err -> Nothing
        Right pat ->
            let matches = generateMatches pat
                filtered = filter validLength matches
             in if null filtered
                    then Nothing
                    else Just (Gen.element filtered)
  where
    validLength txt =
        let len = T.length txt
         in maybe True (len >=) minLen && maybe True (len <=) maxLen

-- | Parse a regex string into a Pattern, normalizing it for generation.
parsePattern :: String -> Either String Pattern
parsePattern "" = Right PEmpty
parsePattern r = case parseRegex r of
    Right (pat, _) -> Right (normalize pat)
    Left err -> Left (show err)

{- | Normalize a pattern for generation.
Removes backreferences (which we don't support in pure mode).
-}
normalize :: Pattern -> Pattern
normalize p = case p of
    PGroup midx inner -> PGroup midx (normalize inner)
    PQuest inner -> PQuest (normalize inner)
    PPlus inner -> PPlus (normalize inner)
    PStar greedy inner -> PStar greedy (normalize inner)
    POr ps -> POr (map normalize ps)
    PConcat ps -> PConcat (map normalize ps)
    PBound lo hi inner -> PBound lo hi (normalize inner)
    -- Backreferences become empty (we don't support them)
    PEscape _ c
        | c `elem` ['1' .. '9'] -> PEmpty
        | otherwise -> p
    other -> other

{- | Generate all matching strings for a pattern (up to a limit).
Uses LogicT for fair enumeration.
-}
generateMatches :: Pattern -> [Text]
generateMatches pat = observeMany 1000 (run pat)

{- | Generate matching strings from a text pattern.
Convenience function for testing.
-}
generateMatchesFromText :: Text -> [Text]
generateMatchesFromText patternText =
    case parsePattern (T.unpack patternText) of
        Left _ -> []
        Right pat -> generateMatches pat

-- | Run the pattern generator in the Logic monad.
run :: Pattern -> Logic Text
run p = case p of
    PEmpty -> pure T.empty
    PChar _ c -> isChar c
    PAny _ (PatternSet (Just cset) _ _ _) -> each $ map T.singleton $ toList cset
    PAny{} -> each $ map T.singleton ['a' .. 'z']
    PAnyNot _ (PatternSet (Just cset) _ _ _) -> chars $ notChars $ toList cset
    PAnyNot{} -> chars $ notChars []
    PQuest inner -> pure T.empty <|> run inner
    PPlus inner -> run $ PBound 1 Nothing inner
    PStar _ inner -> run $ PBound 0 Nothing inner
    PBound low high inner -> do
        n <- each [low .. fromMaybe (low + maxRepeat) high]
        fmap T.concat (replicateM n (run inner))
    PConcat ps -> fmap T.concat (mapM run ps)
    POr xs -> foldr ((<|>) . run) empty xs
    PDot{} -> chars $ notChars []
    PEscape _ c -> chars $ expandEscape c
    PCarat{} -> pure T.empty -- Anchor: zero-width match
    PDollar{} -> pure T.empty -- Anchor: zero-width match
    PGroup _ inner -> run inner
    _ -> pure T.empty -- Unsupported patterns produce empty string
  where
    isChar = pure . T.singleton
    chars = each . map T.singleton
    notChars = ([' ' .. '~'] \\)
    expandEscape ch = case ch of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        'f' -> "\f"
        'a' -> "\a"
        'e' -> "\ESC"
        'd' -> ['0' .. '9']
        'w' -> ['0' .. '9'] ++ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
        's' -> "\t\n\r\f "
        'D' -> notChars ['0' .. '9']
        'W' -> notChars $ ['0' .. '9'] ++ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']
        'S' -> notChars "\t\n\r\f "
        c -> [c]

-- | Choose one element from a list.
each :: (Alternative f) => [a] -> f a
each = foldr ((<|>) . pure) empty
