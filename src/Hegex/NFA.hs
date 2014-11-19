
module Hegex.NFA ( assemble ) where

import           Hegex.Type
import           Control.Monad.State
import qualified Data.Map            as Map
import qualified Data.Set            as Set

type Searcher = (StateNumber, NFATrans)
type Range    = (Int, Int)

assemble :: Tree -> ENFA
assemble tree = NFA { nfaInit   = beg,
                      nfaTrans  = trans,
                      nfaAccept = Set.singleton end }
    where ((beg, end), (_, trans)) = runState (branch tree) (0, Map.empty)

branch :: Tree -> State Searcher Range
branch (TCharacter char) = assembleChar char
branch (TConcat tr1 tr2) = assembleConcat tr1 tr2
branch (TStar tr)        = assembleStar tr
branch (TUnion tr1 tr2)  = assembleUnion tr1 tr2

assembleChar :: Maybe Char -> State Searcher Range
assembleChar char = state connect
    where connect (maxN, trans) = ((beg, end), (end, insertArrow beg char end trans))
              where beg = maxN + 1
                    end = maxN + 2

assembleConcat :: Tree -> Tree -> State Searcher Range
assembleConcat tr1 tr2 = do
  (b1, e1) <- branch tr1
  (b2, e2) <- branch tr2
  (maxN, trans) <- get
  put (maxN, insertArrow e1 Nothing b2 trans)
  return (b1, e2)
  
assembleStar :: Tree -> State Searcher Range
assembleStar tr = do
  (b, e) <- branch tr
  (maxN, trans) <- get
  let newBeg = (maxN+1)
      newEnd = (maxN+2)
      newTrans = insertArrow newBeg Nothing b .
                 insertArrow newBeg Nothing newEnd .
                 insertArrow e Nothing newEnd .
                 insertArrow newEnd Nothing newBeg $ trans
  put(newEnd, newTrans)
  return (newBeg, newEnd)
                        
assembleUnion :: Tree -> Tree -> State Searcher Range
assembleUnion tr1 tr2 = do
  (b1, e1) <- branch tr1
  (b2, e2) <- branch tr2
  (maxN, trans) <- get
  let newBeg = (maxN+1)
      newEnd = (maxN+2)
      newTrans = insertArrow newBeg Nothing b1 .
                 insertArrow newBeg Nothing b2 .
                 insertArrow e1 Nothing newEnd .
                 insertArrow e2 Nothing newEnd $ trans
  put (newEnd, newTrans)
  return (newBeg, newEnd)

insertArrow :: StateNumber -> Maybe Char -> StateNumber -> NFATrans -> NFATrans
insertArrow begin char end trans
    | Map.member begin trans = Map.adjust (Map.insertWith Set.union char end') begin trans
    | otherwise              = Map.insert begin (Map.singleton char end') trans
    where end' = Set.singleton end
