
module Hegex.NFA ( assemble ) where

import           Hegex.Type
import           Control.Monad.State
import qualified Data.Map            as Map

insertArrows :: StateNumber -> Maybe Char -> [StateNumber] -> NFATrans -> NFATrans
insertArrows begin char ends trans
    | Nothing <- Map.lookup begin trans = Map.insert begin (Map.singleton char ends) trans
    | otherwise = Map.update function begin trans
    where function map' = Just $ Map.insertWith (++) char ends map'

assemble :: Tree -> NFA
assemble tree = NFA beg trans [end]
    where ((beg, end), (_, trans)) = runState (branch tree) (0, Map.empty)

branch :: Tree -> State Searcher Range
branch (TCharacter char) = assembleChar char
branch (TConcat tr1 tr2) = assembleConcat tr1 tr2
branch (TStar tr)        = assembleStar tr
branch (TUnion tr1 tr2)  = assembleUnion tr1 tr2

assembleChar :: Maybe Char -> State Searcher Range
assembleChar char = state connect
    where connect (maxN, trans) = ((beg, end), (end, insertArrows beg char [end] trans))
              where beg = maxN + 1
                    end = maxN + 2

assembleConcat :: Tree -> Tree -> State Searcher Range
assembleConcat tr1 tr2 = do
  (b1, e1) <- branch tr1
  (b2, e2) <- branch tr2
  (maxN, trans) <- get
  put (maxN, insertArrows e1 Nothing [b2] trans)
  return (b1, e2)
  
assembleStar :: Tree -> State Searcher Range
assembleStar tr = do
  (b, e) <- branch tr
  (maxN, trans) <- get
  let newBeg = (maxN+1)
      newEnd = (maxN+2)
      newTrans = insertArrows newBeg Nothing [b, newEnd] .
                 insertArrows e Nothing [newEnd] .
                 insertArrows newEnd Nothing [newBeg] $ trans
  put(newEnd, newTrans)
  return (newBeg, newEnd)
                        
assembleUnion :: Tree -> Tree -> State Searcher Range
assembleUnion tr1 tr2 = do
  (b1, e1) <- branch tr1
  (b2, e2) <- branch tr2
  (maxN, trans) <- get
  let newBeg = (maxN+1)
      newEnd = (maxN+2)
      newTrans = insertArrows newBeg Nothing [b1, b2] .
                 insertArrows e1 Nothing [newEnd] .
                 insertArrows e2 Nothing [newEnd] $ trans
  put (newEnd, newTrans)
  return (newBeg, newEnd)
