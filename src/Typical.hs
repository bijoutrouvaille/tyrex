{-# Language OverloadedStrings #-}
-- Typical Expression Engine
-- Loosely based on "Regular Expression Matching Can Be Simple And Fast" by Russ Cox.
-- https://swtch.com/~rsc/regexp/regexp1.html

-- in simple words: this is a type-safe regex engine

module Typical
    ( Quantifier
    , Class
    , Pattern (..)
    , _word
    , _oneOf
    , _digit
    , _real
    , _alpha
    , _lower
    , _upper
    , _char
    , _whitespace
    , _anything
    , _one
    , _any
    , _some
    , _optional
    , _exactly
    , _not
    , _or
    , _seq
    , (=~)
    , match
    , matchWithRemainder
    , examplePattern
    ) where

-- Structure:
-- The module consists of 2 blocks: 
-- * pattern definition syntax
-- * matching functions with result types

-- Pattern Syntax
-- The data type required by the match function is Pattern

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Functor (Functor)
import Debug.Trace (trace)

data Class = SmallAlpha | CapAlpha | Digit | Anything | Specific Char | ClassUnion [Class] deriving (Show, Eq)
data Delimiter = Start | End deriving (Show, Eq)
data Quantifier = Between Int (Maybe Int)
  deriving (Show, Eq)

data Pattern = 
  Sequence [Pattern] 
  | Disjunction [Pattern] 
  | Quantified Quantifier Pattern 
  | Delimited Delimiter Pattern
  | Atom Class  
  deriving (Show, Eq)

data MatchItem = MatchItem { matchPiece :: String, remainder :: String }
  deriving (Show, Eq)
data MatchTree = MatchTop MatchItem | MatchTree MatchItem [MatchTree] 
  deriving (Show, Eq)

noMatch = MatchTree (MatchItem "" "") []

(=~) str patt = not . null . match patt $ str

flattenTree :: MatchTree -> [(String, String)]
flattenTree (MatchTop item) = [(matchPiece item, remainder item)]
flattenTree (MatchTree (MatchItem {matchPiece=matchPiece, remainder=remainder}) children) = result where
  result = [ (matchPiece++m, r)  | (m, r) <- flattenedChildren ]
  f = \tree acc -> acc++(flattenTree tree)
  flattenedChildren = foldr f [] children

fromMaybe d Nothing = d
fromMaybe _ (Just m) = m

_start = Start
_end = End
_seq = Sequence

_word :: String -> Pattern
_word = Sequence . map (Atom . Specific)

_exactly n = Quantified (Between n (Just n))
_between min max = Quantified . Between min $ Just max
_one = _exactly 1
_some = Quantified (Between 1 Nothing)
_any = Quantified (Between 0 Nothing)
_optional = Quantified (Between 0 (Just 1))
_not = Quantified (Between 0 (Just 0))

_char = Atom . Specific
_digit = Atom Digit
_alpha = Atom (ClassUnion [SmallAlpha, CapAlpha])
_lower = Atom SmallAlpha
_upper = Atom CapAlpha
_anything = Atom Anything
_oneOf = Atom . ClassUnion . map Specific
_whitespace = _oneOf " \t\n\r" 

_real = _seq [ _optional (_char '-'), _some _digit, _optional . _seq $ [_char '.', _some _digit] ]

_or :: Pattern -> Pattern -> Pattern
left `_or` right = Disjunction ( nodes left ++ nodes right ) where
  nodes (Disjunction r) = r
  nodes r = [r]

examplePattern = 
  [  _digit `_or` _char '∞' `_or` _word "po"
  , _some _whitespace

  , _one ( _word "fun" )
  , _word "latte" `_or` _word "mocha"
  , _exactly 3 (_char '☻')
  ]

isAtom :: Class->Char->Bool
isAtom SmallAlpha c = Char.isLower c
isAtom CapAlpha c = Char.isUpper c
isAtom Digit c = Char.isDigit c
isAtom (Specific s) c = c==s
isAtom Anything c = True
isAtom (ClassUnion classes) c = any (\x->isAtom x c) $ classes

match :: [Pattern] -> String -> [String]
match pattern = fmap fst . matchWithRemainder pattern

matchWithRemainder :: [Pattern] -> String -> [(String, String)]
matchWithRemainder p s = matchAll (_seq p) s
  where
    matchAll :: Pattern -> String -> [(String, String)]
    matchAll p "" = [] 
    matchAll p this@(_:cs) = 
      let ms = flattenTree $ matchPattern p (MatchTop $ MatchItem "" this)
          ns = matchAll p cs
       in ms ++ ns

    clean (MatchTree item children) = 
      let filtered = [ c | c <- children, c /= noMatch ]
          tree [] = noMatch
          tree ms = MatchTree item ms
       in tree filtered
    clean x = x

    matchPattern :: Pattern -> MatchTree -> MatchTree
    matchPattern pattern (MatchTree item children) = 
      let matched = [ matchPattern pattern c | c <- children ]
       in clean $ MatchTree item matched
    matchPattern (Atom cls) (MatchTop item) = 
      let str = remainder item
          tree char str = MatchTree item [MatchTop $ MatchItem [char] str]
          shiftTop cs = MatchTop (MatchItem "" cs)
          next [] = noMatch
          next (c:cs) = if isAtom cls c then tree c cs else noMatch
       in next str
    matchPattern (Disjunction ps) parent@(MatchTop item) = 
      let children = [ matchPattern pattern parent | pattern <- ps] 
          empty (MatchItem m r) = MatchItem "" r
       in clean $ MatchTree (empty item) children
    matchPattern (Quantified (Between min maybeMax) pattern) parent = 
      let inRange n = min <= n && n <= (fromMaybe n maybeMax)
          try (Just prev) (MatchTree _ []) n = if inRange (n-1) then prev else noMatch
          try _ curr n = try (Just curr) (matchPattern pattern curr) (n+1)
       in try Nothing parent 0
    matchPattern (Sequence []) parent = parent
    matchPattern (Sequence (p:ps)) parent@(MatchTop item) = 
      matchPattern (Sequence ps) (matchPattern p parent)
