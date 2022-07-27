module MiniLang.Parser.AST.ExprSpec where

import Test.Hspec
import MiniLang.Parser (parse)
import MiniLang.Parser.AST.Expr
import MiniLang.Data.AST.Expr (Value(Value))

spec :: Spec
spec = do
  describe "value" $ do
    context "when String has just 1 letter" $ do
      context "when the letter is not number" $ do
        it "fail to parse" $ do
          parse value "a" `shouldBe` Nothing
      context "when the letter is number" $ do
        it "return Value" $ do
          parse value "0" `shouldBe` Just (Value 0, "")
