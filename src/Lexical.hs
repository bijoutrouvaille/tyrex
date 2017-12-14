{-# Language OverloadedStrings #-}
module Lexical
    ( example
    , parse
    ) where

import Typical
number = _real
variable = _seq [ _some _alpha, _any ( _alpha `_or` _digit `_or` _oneOf "_$") ]
strange = _some . _oneOf $ "bcn0"
operator = _oneOf "+-/*^&|<>$#@!~"
whitespace = _some _whitespace
grouping = _oneOf "[](){}"

data LexemeType = Number | Variable | Operator | Strange | Whitespace | Grouping deriving (Show)
data RegisterEntry = RegisterEntry Pattern LexemeType 
register :: [RegisterEntry]
register = [
  RegisterEntry number Number,
  RegisterEntry operator Operator,
  RegisterEntry variable Variable,
  RegisterEntry grouping Grouping,
  RegisterEntry whitespace Whitespace,
  RegisterEntry strange Strange
  ] 
data Lexeme = Lexeme String LexemeType deriving (Show)


data ResultNode = ResultNode Lexeme ResultTree | ResultFail deriving (Show)
data ResultTree = ResultTree [ResultNode] | TreeTop deriving (Show)

deepParse :: String -> ResultTree
deepParse [] = TreeTop
deepParse input = ResultTree (fmap toTree nextStage)
  where nextStage = getNextLexeme input
        toTree Impossible = ResultFail
        toTree (Possible lexeme restOfInput) = 
          ResultNode lexeme (deepParse restOfInput)

getNextLexeme :: String -> [LexemeNode]
getNextLexeme input = possibilities
  where try :: RegisterEntry -> LexemeNode
        try reg = extractLexeme input reg
        attempts :: [LexemeNode]
        attempts = fmap try register
        possibilities :: [LexemeNode]
        possibilities = filter onlyPossible attempts
        onlyPossible :: LexemeNode -> Bool
        onlyPossible Impossible = False
        onlyPossible _ = True

data LexemeNode = Possible Lexeme String | Impossible
extractLexeme :: String -> RegisterEntry -> LexemeNode
extractLexeme input (RegisterEntry regex typ) = node
  where node = lex $ matchWithRemainder [regex] input
        lex [] = Impossible
        lex ((match, remainder):_) = Possible (Lexeme match typ) remainder  
        
parse :: String -> ResultTree
parse input = deepParse input

flatten :: ResultTree -> [ [Lexeme] ]
flatten TreeTop = []
flatten (ResultTree nodes) = concat $ map unwrap nodes where
  unwrap :: ResultNode -> [ [Lexeme ] ]
  unwrap ResultFail = []
  unwrap (ResultNode lexeme TreeTop) = [[lexeme]]
  unwrap (ResultNode lexeme tree) = map (lexeme:) (flatten tree)

prettyFlat :: [ [ Lexeme ] ] -> String
prettyFlat = unlines . map (pad . show) where
  pad = ("  "++)

example :: IO ()
example = do
  putStrLn "Enter program"
  input <- getLine 
  putStr . prettyFlat . flatten . parse $ input

