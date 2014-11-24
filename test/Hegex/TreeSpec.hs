
module Hegex.TreeSpec ( spec ) where

import SpecHelper

spec :: Spec
spec = do
  describe "build a syntax tree from a regular expression" $ do
         it "convert 'a(b|)*c" $
            buildTree "a(b|)*c"
                          `shouldBe` (TConcat
                                      (TConcat
                                       (TCharacter (Just 'a'))
                                       (TStar
                                        (TUnion (TCharacter (Just 'b'))
                                                (TCharacter Nothing))))
                                      (TCharacter (Just 'c')))

         it "convert ''" $
            buildTree ""
                      `shouldBe` (TCharacter Nothing)

         it "convert 'a|*'" $ do
            evaluate (buildTree "a|*")
                      `shouldThrow` errorCall "Syntax error : Bad Pattern."

         it "convert 'a(bc))d'" $ do
            evaluate (buildTree "a(bc))d")
                      `shouldThrow` errorCall "Syntax error : Bad Parenthesis."
