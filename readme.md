# Tyrex

A non-deterministic typical expressions parser for Haskell language. A typical expression is a type-safe regular expression.

Other proposed names include: typex (typical expressions), texen (typical expressions engine).

## Usage

`import Typical`

gives you many functions that correspond to standard regular expressions operators. To parse a string use a `match :: [Pattern] -> [String]`. For example:

- `match [_digit, _char '+', _digit] "4+5"`
- ``match [_digit `_or` _alpha] "a1"``
- ``match [_digit, _optional . _seq $ [ _char '(', _word "one", _char ')' ] ] "one"``


The result will be an array of strings that represents a list of possible matches. Match takes an array of patterns for convenience. Internally, it converts the array into the `Sequence` structure.

### Types overview

The most important data type is called `Pattern`. It is recursive and has a number of data constructors. By applying it to the `match` function you get a list of possible matches.

### Positioning Operators

- `_seq :: [Pattern] -> Sequence [Pattern]` - converts an array of patterns into a sequence pattern.
- `_or :: Pattern -> Pattern -> Pattern` - converts two patterns into a disjunction.

### Qualifier Operators

- `_char :: Char -> Pattern` - converts a character into a pattern that matches it.
- `_digit :: Pattern` - matches any digit.
- `_alpha :: Pattern` - matches one upper or lower case letter.
- `_lower :: Pattern` - matches a lower case letter.
- `_upper :: Pattern` - matches upper case letter.
- `_anything :: Pattern` - matches any character.
- `_oneOf :: [Char] -> Pattern` - matches any one of the characters in the passed array.
- `_whitespace = _oneOf " \t\n\r" `
- `_real :: Pattern` - matches a positive or negative real number.

### Quantifier Operators

- `_exactly :: Num -> Pattern -> Pattern` - matches a passed pattern exactly n times or fails.
- `_between :: Num -> Num -> Pattern` - greedily matches a pattern between min and max times.
- `_one = _exactly 1`
- `_some :: Pattern -> Pattern` - greedily matches a pattern at least once.
- `_any :: Pattern -> Pattern` - greedily matches a pattern zero or more times.
- `_optional :: Pattern -> Pattern` - greedily matches zero or one times.
- `_not :: Pattern -> Pattern` - matches 0 times or fails.

## Work in Progress

Plans to convert this to a monadic parser are on the way. 
This parser still lacks important features, like `^` and `$`.
A non-deterministic lexer is also included in the `Lexical` package.
