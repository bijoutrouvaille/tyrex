module LexicalSpec (main, spec) where

import Test.Hspec
import Lexical

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Lexical" $ do
  it "does nothing" $
    shouldBe 1 1

