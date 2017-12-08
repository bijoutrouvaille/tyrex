import Test.Hspec
import Typical

shouldBeNot = shouldBe . not

main :: IO ()
main = hspec $ do
  describe "Typical" $ do

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

    context "quantified exactly" $ do
      it "matches a word twice" $ 
        match [ _exactly 2 $ _word "hello" ] "0hellohello*" `shouldBe` ["hellohello"]
      it "matches a sub-sequence twice" $
        match 
        [ _word "hello "
        , _exactly 2 . _seq $ [_word "world"]
        , _char '!'
        ] ":hello worldworld!:)" `shouldBe` ["hello worldworld!"]

    -- it "matches class union digit char" $
    --   (match [_classes [_char, _digit]] "#b1$Y0") `shouldBe` ["b1", "Y0"]
