--{-# OPTIONS_GHC -fno-warn-unused-do-bind -F -pgmF hspec-discover #-}
import Test.Hspec

import RLanguage
import RParser

main :: IO ()
main = hspec $ do
  describe "R - Constants" $ do
    it "Parses NULL" $ do
      parseRConstant "NULL" `shouldBe` (Right RNull)
    it "Parses Numeric" $ do
      parseRConstant "<NUMERIC>" `shouldBe` (Right (RNumeric 1.0))
    it "Parses String With escaped quote" $ do
      parseRConstant "'He says: \\'Hello World\\''" `shouldBe` (Right (RString "He says: 'Hello World'"))
    it "Parses String with escaped tab" $ do
      parseRConstant "'He says: \\'Hello\\tWorld\\''" `shouldBe` (Right (RString "He says: 'Hello\\tWorld'"))
    it "Parses String with escaped \\" $ do
      parseRConstant "'He says: \\'Hello\\\\World\\''" `shouldBe` (Right (RString "He says: 'Hello\\\\World'"))

  describe "R - Expressions" $ do
    it "Parses an empty string " $ do
      parseRExpression "" `shouldBe` (Right (REndOfFile))
    it "Parses an empty line " $ do
      parseRExpression "\n" `shouldBe` (Right (RWhitespace))
    it "Parses a empty function call" $ do
      parseRExpression "foo()\n" `shouldBe` (Right (FunctionCall (RVariableExpression (RIdentified "foo")) []))
    it "Parses a function call with an argument" $ do
      parseRExpression "foo(bar)\n" `shouldBe` (Right (FunctionCall (RVariableExpression (RIdentified "foo")) [(RSimpleFunctionArgument $ RVariableExpression (RIdentified "bar"))]))

  describe "R - Files" $ do
    it "Parses an empty line " $ do
      parseR "" `shouldBe` (Right [])
    it "Parses 2 lines of function calls" $ do
      parseR "foo()\nbar()\n" `shouldBe` (Right [FunctionCall (RVariableExpression (RIdentified "foo")) [], FunctionCall (RVariableExpression (RIdentified "bar")) []])

  describe "R - Assignment" $ do
    it "Parses a simple  = assignment" $ do
      parseR "variable = \"Hello\";" `shouldBe` (Right [(RAssignment (RVariableExpression (RIdentified "variable")) REqualSign(RConstantExpression (RString "Hello")))])
    it "Parses a simple <- assignment" $ do
      parseR "variable <- \"Hello\";" `shouldBe` (Right [(RAssignment (RVariableExpression (RIdentified "variable")) RArrow (RConstantExpression (RString "Hello")))])
