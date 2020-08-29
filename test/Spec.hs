{-# OPTIONS_GHC -fno-warn-unused-do-bind -F -pgmF hspec-discover #-}
import Test.Hspec

import RParser2

main :: IO ()
main = hspec $ do
  describe "R - Constants" $ do
    it "Parses NULL" $
      
