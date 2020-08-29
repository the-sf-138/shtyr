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
    it "Parses String" $ do
      parseRConstant "'HelloWorld'" `shouldBe` (Right (RString "HelloWorld"))
      
