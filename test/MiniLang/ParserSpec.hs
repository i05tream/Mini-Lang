module MiniLang.ParserSpec where

import Data.Char (toUpper)
import Control.Applicative ((<|>))
import Test.Hspec
import MiniLang.Parser

spec :: Spec
spec = do
  describe "item" $ do
    context "when non-blank String is passed" $ do
      it "succeed in parsing" $ do
        parse item "hello" `shouldBe` Just('h', "ello")

    context "when blank String is passed" $ do
      it "fail to parsing" $ do
        parse item "" `shouldBe` Nothing

  describe "match" $ do
    context "when String matches predicate" $ do
      it "succeed in parsing" $ do
        parse (match (== 'H')) "Hello" `shouldBe` Just('H', "ello")

    context "when String doesn't match predicate" $ do
      it "fail to parse" $ do
        parse (match (== 'H')) "hello" `shouldBe` Nothing

  describe "char" $ do
    context "when succeed in parsing" $ do
      it "return Char And rest String" $ do
        parse (char 'h') "hello" `shouldBe` Just ('h', "ello")

    context "when fail to parse" $ do
      it "return Nothing" $ do
        parse (char 'h') "good bye" `shouldBe` Nothing

  describe "digit" $ do
    context "when digit is passed" $ do
      it "succeed in parsing and return Integer" $ do
        parse digit "1ab" `shouldBe` Just (1, "ab")

  describe "number" $ do
    context "when String starts with digits" $ do
      context "when digits are valid number" $ do
        it "succeed in parsing and return Integer" $ do
          parse number "12ab" `shouldBe` Just (12, "ab")
      context "when digits are invalid number" $ do
        it "fail to parsing" $ do
          parse number "01ab" `shouldBe` Nothing

  describe "fmap" $ do
    let parser = fmap toUpper (char 'h')
    context "when succeed in parsing" $ do
      it "apply function to return value" $ do
        parse parser "hello" `shouldBe` Just ('H', "ello")
    context "when fail to parse" $ do
      it "return Nothing" $ do
        parse parser "good bye" `shouldBe` Nothing

  describe "<*>" $ do
    context "when succeed in parsing" $ do
      it "" $ do
        parse (Parser (\(c : cs) -> Just (toUpper, cs)) <*> item) "abc" `shouldBe` Just ('B', "c")

    context "when fail to parse" $ do
      it "" $ do
        parse (Parser (\_ -> Nothing) <*> item) "abc" `shouldBe` (Nothing :: Maybe (Char, String))

  describe "return" $ do
    it "return Parser" $ do
      parse (return 1) "Haskell" `shouldBe` Just (1, "Haskell")

  describe ">>=" $ do

    it "apply item twice" $ do
      parse (item >>= \c -> item >>= \c' -> return [c, c']) "Haskell" `shouldBe` Just ("Ha", "skell")

  describe "<|>" $ do
    let parseA = char 'A'
        parseB = char 'B'
    context "when either parseA or parseB matches" $ do
      it "succeed in parsing" $ do
        parse (parseA <|> parseB) "A" `shouldBe` Just ('A', "")
    context "when neither parseA nor parseB matches" $ do
      it "failed to parse" $ do
        parse (parseA <|> parseB) "C" `shouldBe` Nothing
