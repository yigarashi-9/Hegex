
module Hegex.NFASpec ( spec ) where

import           SpecHelper
import qualified Data.Map   as Map
import qualified Data.Set   as Set

spec :: Spec
spec = do
  describe "build a NFA from syntax tree." $ do

         it "convert the tree of '(a*|bc)'" $
            assemble case1 `shouldBe` result1

      where case1   = TUnion
                      (TStar (TCharacter (Just 'a')))
                      (TConcat (TCharacter (Just 'b'))
                               (TCharacter (Just 'c')))

            result1 = NFA { nfaInit   = 9,
                            nfaTrans  = Map.fromList [(1,Map.fromList [(Just 'a',[2])]),
                                                      (2,Map.fromList [(Nothing,[4])]),
                                                      (3,Map.fromList [(Nothing,[1,4])]),
                                                      (4,Map.fromList [(Nothing,[10,3])]),
                                                      (5,Map.fromList [(Just 'b',[6])]),
                                                      (6,Map.fromList [(Nothing,[7])]),
                                                      (7,Map.fromList [(Just 'c',[8])]),
                                                      (8,Map.fromList [(Nothing,[10])]),
                                                      (9,Map.fromList [(Nothing,[3,5])])],
                            nfaAccept = Set.fromList [10]}

main :: IO ()
main = hspec spec
