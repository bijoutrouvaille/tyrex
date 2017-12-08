{-# Language OverloadedStrings #-}
-- Typical Expression Engine
-- Thompson Optimized Matching Machine
-- Ken Thompson nondeterministic automaton kit for construting 
-- fast and type safe character pattern searches.
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
    , _alpha
    , _lower
    , _upper
    , _char
    , _whitespace
    , _anything
    , _one
    , _any
    , _optional
    , _exactly
    , _not
    , _or
    , _seq
    , match
    , examplePattern
    , example
    ) where

-- Structure:
-- The module consists of 2 blocks: 
-- * pattern definition syntax
-- * matching functions with result types

-- Pattern Syntax
-- The data type required by the match function is Patter, 
-- which is composed of Class and Quantifier types.

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Functor (Functor)
import Debug.Trace (trace)

data Class = SmallAlpha | CapAlpha | Digit | Anything | Specific Char | ClassUnion [Class] deriving Show
data Delimiter = Start | End deriving Show
data Quantifier = Between Int (Maybe Int)
  deriving Show

data Pattern = 
  Sequence [Pattern] 
  | Disjunction [Pattern] 
  | Quantified Quantifier Pattern 
  | Delimited Delimiter Pattern
  | Atom Class  
  deriving Show

data MatchItem = MatchItem { matchPiece :: String, remainder :: String }
  deriving (Show, Eq)
data MatchTree = MatchTop MatchItem | MatchTree MatchItem [MatchTree] | NoMatch
  deriving (Show, Eq)

tree item children = 
  let clean [] = NoMatch
      clean ms = MatchTree item [ m | m <- ms, m /= NoMatch ]
   in clean children

flattenTree :: MatchTree -> [String]
flattenTree NoMatch = [] 
flattenTree (MatchTop item) = [matchPiece item]
flattenTree (MatchTree (MatchItem {matchPiece=matchPiece}) children) = result where
  result = [ matchPiece++c | c <- flattenedChildren ]
  f = \tree acc -> acc++(flattenTree tree)
  flattenedChildren = foldr f [] children

if' b q p = if b then q else p

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
match p s = shiftMatch (Sequence p) s
  where

    shiftMatch :: Pattern -> String -> [String]
    shiftMatch p "" = [] 
    shiftMatch p this@(_:next) = (flattenTree $ matchPattern p (MatchTop $ MatchItem "" this)) ++ shiftMatch p next

    matchPattern :: Pattern -> MatchTree -> MatchTree
    matchPattern pattern NoMatch = NoMatch
    matchPattern pattern (MatchTree (MatchItem {remainder=""}) children) = NoMatch
    matchPattern pattern (MatchTree item children) = 
      let matched = [ matchPattern pattern c | c <- children, c /= NoMatch ]
          filtered = [ c | c <- matched, c/=NoMatch ]
          tree [] = NoMatch
          tree ms = MatchTree item ms
       in tree filtered
    matchPattern (Atom cls) (MatchTop item) = 
      let str = remainder item
          tree char str = MatchTree item [MatchTop $ MatchItem [char] str]
          next [] = NoMatch
          next (c:cs) = if isAtom cls c then tree c cs else NoMatch
       in next str
    matchPattern (Disjunction ps) parent@(MatchTop item) = 
      let children = [ matchPattern pattern parent | pattern <- ps] 
       in MatchTree item children
    matchPattern (Quantified (Between min maybeMax) pattern) parent = 
      let inRange n = min <= n && n <= (fromMaybe n maybeMax)
          try Nothing NoMatch n = NoMatch
          try (Just prev) NoMatch n = if inRange (n-1) then prev else NoMatch
          try _ curr n = try (Just curr) (matchPattern pattern curr) (n+1)
       in try Nothing parent 0
    matchPattern (Sequence []) parent = parent
    matchPattern (Sequence (p:ps)) parent@(MatchTop item) = matchPattern (Sequence ps) (matchPattern p parent)
    matchPattern _ _ = NoMatch

example :: IO ()
example = do
  putStr . show . match [ _word "tun" ] $ "atun"

