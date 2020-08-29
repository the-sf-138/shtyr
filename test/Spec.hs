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
    it "Parses an empty function call" $ do
      parseR "foo()" `shouldBe` (Right (FunctionCall (RFunctionIdentifier (RIdentified "foo")) []))
