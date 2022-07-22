module MiniLang.ParserSpec where

import Data.Char (toUpper)
import Test.Hspec
import MiniLang.Parser

spec :: Spec
spec = do
  describe "char" $ do
    context "when succeed in parsing" $ do
      it "return Char And rest String" $ do
        parse (char 'h') "hello" `shouldBe` Just ('h', "ello")

    context "when fail to parse" $ do
      it "return Nothing" $ do
        parse (char 'h') "good bye" `shouldBe` Nothing

  describe "fmap" $ do
    let parser = fmap toUpper (char 'h')
    context "when succeed in parsing" $ do
      it "apply function to return value" $ do
        parse parser "hello" `shouldBe` Just ('H', "ello")
    context "when fail to parse" $ do
      it "return Nothing" $ do
        parse parser "good bye" `shouldBe` Nothing

  describe "pure" $ do
    it "return Parser" $ do
      parse (pure 1) "Haskell" `shouldBe` Just (1, "Haskell")
