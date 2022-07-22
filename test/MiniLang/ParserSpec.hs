module MiniLang.ParserSpec where

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
