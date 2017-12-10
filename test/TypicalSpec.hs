module TypicalSpec (main, spec) where

import Test.Hspec
import Typical

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Typical" $ do

    it "matches the entire word" $
      match [_word "operose"] "operose" `shouldBe` ["operose"]

    it "matches a sub-word" $
      match [_word "pac"] "space" `shouldBe` ["pac"]
    it "matches digit" $
      match [_digit] "a3" `shouldBe` ["3"]
    it "matches digit and word" $
      match [_digit, _word "clap"] "3clap" `shouldBe` ["3clap"]
    
    it "matches multiple sub words" $
      match [_word "ac"] "1pac2pac" `shouldBe` ["ac", "ac"]

    it "matches multiple sub-digit-and-words" $
      match [_digit, _word "ac"] "1ac2ac" `shouldBe` ["1ac", "2ac"]

    it "matches anything" $
      match [_anything] "abc" `shouldBe` ["a","b","c"]

    it "does not match an incomplete pattern" $
      match [ _digit, _digit ] "1" `shouldBe` []

    it "matches a simple disjunction" $
      match [_digit `_or` _lower] "f" `shouldBe` ["f"]
    it "matches a digit in word|digit disjunction" $
      match [_word "operiment" `_or` _digit] "1" `shouldBe` ["1"]
    it "matches a word in word|digit disjunction" $
      match [_word "operiment" `_or` _digit] "$operiment$" `shouldBe` ["operiment"]

    it "matches an optional pattern" $
      match [_word "hello", _optional _digit, _char '!'] "hello1!" `shouldBe` ["hello1!"]


    it "matches a variable name" $
      match 
      [ _some _alpha
      , _any ( _alpha `_or` _digit `_or` (_oneOf "_$")) 
      ] "te_St$1" `shouldBe` [ "te_St$1", "e_St$1" ,"St$1" ,"t$1" ]

    context "disjunction" $ do
      it "matches single letter _digit _or _alpha" $
        match [ _digit `_or` _alpha ] "a" `shouldBe` ["a"]
      it "matches single digit _digit _or _alpha" $
        match [ _digit `_or` _alpha ] "1" `shouldBe` ["1"]

      context "quantified" $ do
        it "matches _any (_digit _or _alpha)" $
          match [ _any (_digit `_or` _alpha) ] "a1b2" `shouldBe` ["a1b2", "1b2", "b2", "2"]

    context "_alpha" $ do
      it "matches _some" $
        match [ _some _alpha ] "one_two" `shouldBe` ["one", "ne", "e", "two", "wo", "o"]

    -- context "_any" $ do
    --   it "matches any digit" $
    --     match [ _any _digit ] "12" `shouldBe` ["12", "2"]

    context "_exactly" $ do
      it "matches a word twice" $ 
        match [ _exactly 2 $ _word "hello" ] "0hellohello*" `shouldBe` ["hellohello"]
      it "matches a sub-sequence twice" $
        match 
        [ _word "hello "
        , _exactly 2 . _seq $ [_word "world"]
        , _char '!'
        ] ":hello worldworld!:)" `shouldBe` ["hello worldworld!"]
    context "_real" $ do
      it "matches PI" $ 
        match [_real] "3.14" `shouldBe` ["3.14", "14", "4"]
      it "matches between words" $
        match [_word "hello ", _real, _word " world"] "hello 3.14 world" `shouldBe` ["hello 3.14 world"]

    context "remainder" $ do
      it "returns the rest" $
        matchWithRemainder [_real] " 2hello" `shouldBe` [("2", "hello")]


