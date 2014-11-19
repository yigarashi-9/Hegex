
module Hegex.DFASpec ( spec ) where

import           SpecHelper
import qualified Data.Map   as Map
import qualified Data.Set   as Set

spec :: Spec
spec = describe "build a DFA from ENFA." $ do

         it "convert the NFA of '(a*|ab)'" $
            convert case1 `shouldBe` result1

    where case1 = NFA { nfaInit = 9,
                        nfaTrans = Map.fromList [(1,Map.fromList [(Just 'a',[2])]),
                                                 (2,Map.fromList [(Nothing,[4])]),
                                                 (3,Map.fromList [(Nothing,[1,4])]),
                                                 (4,Map.fromList [(Nothing,[10,3])]),
                                                 (5,Map.fromList [(Just 'a',[6])]),
                                                 (6,Map.fromList [(Nothing,[7])]),
                                                 (7,Map.fromList [(Just 'b',[8])]),
                                                 (8,Map.fromList [(Nothing,[10])]),
                                                 (9,Map.fromList [(Nothing,[3,5])])],
                        nfaAccept = Set.fromList [10]
                      }
                  
          result1 = DFA { dfaInit   = 0,
                          dfaTrans  = Map.fromList [(('a',0),1),(('a',1),2),(('a',2),2),(('b',1),3)],
                          dfaAccept = Set.fromList [0, 1, 2, 3]}
                    
main :: IO ()
main = hspec spec
