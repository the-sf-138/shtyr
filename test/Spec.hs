--{-# OPTIONS_GHC -fno-warn-unused-do-bind -F -pgmF hspec-discover #-}
import Test.Hspec

import RParser2

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
    it "Parses a empty function call" $ do
      parseR "foo()" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo")) []))
    it "Parses a function call with simple named argument" $ do
      parseR "foo(bar)" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo"))
                                           [(RSimpleFunctionArgument $ RIdentifierExpression (RIdentified "bar"))]))
    it "Parses a function call with multiple simple named argument" $ do
      parseR "foo(bar, bar2)" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo"))
                                           [(RSimpleFunctionArgument $ RIdentifierExpression (RIdentified "bar")),
                                             (RSimpleFunctionArgument $ RIdentifierExpression (RIdentified "bar2"))]))
    it "Parses a nested function call" $ do
      parseR "foo(bar()) " `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo"))
                                           [ RSimpleFunctionArgument (FunctionCall (RFunctionIdentifier (RIdentified "bar")) [])]))

    it "Parses a function call with a tagged function argument name" $ do
      parseR "foo(bar=something)" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo"))
                                           [ RTaggedFunctionArgument (RTagIdentifier $ RIdentified "bar")
                                             (RIdentifierExpression (RIdentified "something")) ] ))
    it "Parses a function call with a tagged function argument expression" $ do
      parseR "foo(bar=something())" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo"))
                                           [ RTaggedFunctionArgument (RTagIdentifier $ RIdentified "bar")
                                             (FunctionCall (RFunctionIdentifier $ RIdentified "something") [])] ))
