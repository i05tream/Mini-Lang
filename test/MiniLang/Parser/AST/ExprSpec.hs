module MiniLang.Parser.AST.ExprSpec where

import Test.Hspec
import MiniLang.Parser (parse)
import MiniLang.Parser.AST.Expr
import MiniLang.Data.AST.Expr

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

  describe "add" $ do
    context "when String starts with x+y" $ do
      it "succeed in parsing and return Expr" $ do
        parse add "1+2" `shouldBe` Just (Add (Value 1) (Value 2), "")

  describe "sub" $ do
    context "when String starts with x-y" $ do
      it "succeed in parsing and return Expr" $ do
        parse sub "100-21" `shouldBe` Just (Sub (Value 100) (Value 21), "")

  describe "mul" $ do
    context "when String starts with x*y" $ do
      it "succeed in parsing and return Expr" $ do
        parse mul "11*11" `shouldBe` Just (Mul (Value 11) (Value 11), "")

  describe "div" $ do
    context "when String starts with x*y" $ do
      it "succeed in parsing and return Expr" $ do
        parse div "11*11" `shouldBe` Just (Div (Value 11) (Value 11), "")
