
module Hegex.DFA ( enfa2nfa, convert, simulate ) where

import           Hegex.Type
import           Data.List
import qualified Data.Map   as Map
import qualified Data.Set   as Set

simulate :: DFA -> String -> Bool
simulate (DFA init trans accept) str = loop init str
    where
      loop state []     = Set.member state accept 
      loop state (c:cs) = case Map.lookup (c, state) trans of
                             Just next -> loop next cs
                             Nothing   -> False

notExist :: StateNumber
notExist = -1

convert :: ENFA -> DFA
convert = nfa2dfa . enfa2nfa

nfa2dfa :: NFA -> DFA
nfa2dfa nfa = DFA { dfaInit   = 0,
                    dfaTrans  = dfamTrans dfaMakerRes,
                    dfaAccept = getAcceptList nfa dfaMakerRes }
    where
      dfaMakerInit = DFAMaker { subsetIndexer = Map.singleton (Set.singleton $ nfaInit nfa) 0,
                                dfamCounter   = 0,
                                dfamTrans     = Map.empty }
      dfaMakerRes  = deleteNondetermin nfa dfaMakerInit [nfaInit nfa]


getAcceptList :: NFA -> DFAMaker -> Accepts
getAcceptList nfa dfaMaker = Set.fromList [ i | (subset, i) <- subsetIndices, isAccepted subset ]
    where
      subsetIndices     = Map.assocs (subsetIndexer dfaMaker)
      isAccepted subset = not . Set.null $ Set.intersection (nfaAccept nfa) subset
                                           

deleteNondetermin :: NFA -> DFAMaker -> [StateNumber]-> DFAMaker
deleteNondetermin nfa dfaMaker starts
    = foldl (deleteNondetermin nfa) dfaMaker' unsearched
    where
      translist  = Map.assocs $ unionTransition (nfaTrans nfa) starts
      dfaMaker'  = connectAdjacent translist dfaMaker starts
      unsearched = foldr filterfunc [] translist
      filterfunc (_, list) acc = if lookupFromlist dfaMaker list == notExist
                                 then (list:acc)
                                 else acc

connectAdjacent :: [(Maybe Char, [StateNumber])] -> DFAMaker -> [StateNumber] -> DFAMaker
connectAdjacent translist dfaMaker starts = foldl step dfaMaker translist
    where
      startNum  = lookupFromlist dfaMaker starts
      step (DFAMaker sets cnt dfaTrans) (Just c, dests)
          | num == notExist = (DFAMaker (Map.insert (Set.fromList dests) (cnt+1) sets)
                                      (cnt+1)
                                      (Map.insert (c, startNum) (cnt+1) dfaTrans))
          | otherwise       = (DFAMaker sets
                                        cnt
                                        (Map.insert (c, startNum) num dfaTrans))
          where num = lookupFromlist dfaMaker dests

lookupFromlist :: DFAMaker -> [StateNumber] -> StateNumber
lookupFromlist dfaMaker state = Map.findWithDefault notExist
                                                    (Set.fromList state)
                                                    (subsetIndexer dfaMaker)

enfa2nfa :: ENFA -> NFA
enfa2nfa nfa@(NFA init trans accept) = deleteEpsilonTrans nfa init (NFA init Map.empty Set.empty)

deleteEpsilonTrans :: NFA -> StateNumber -> NFA -> NFA
deleteEpsilonTrans nfa@(NFA init trans acceptset) start nfaAcc@(NFA initAcc transAcc acceptAcc)
    = if Map.member start transAcc
      then nfaAcc
      else foldl (\acc' x -> deleteEpsilonTrans nfa x acc')
                 (NFA initAcc transAcc' acceptAcc')
                 dest
    where etrans     = collectEpsilonTrans trans start
          accept     = head $ Set.toList acceptset
          subtrans   = unionTransition trans etrans
          dest       = concat $ Map.elems subtrans
          acceptAcc' = if accept `elem` etrans
                       then Set.insert start acceptAcc
                       else acceptAcc
          transAcc'  = if Map.null subtrans
                       then transAcc
                       else Map.insert start subtrans transAcc


collectEpsilonTrans :: NFATrans -> StateNumber -> [StateNumber]
collectEpsilonTrans trans start = loop start [start]
    where 
      loop start' accum
          | onlydest == [] = accum
          | otherwise      = foldr (\x acc -> loop x acc) (accum ++ onlydest) onlydest
          where map'      = Map.findWithDefault Map.empty start' trans 
                dest      = Map.findWithDefault [] Nothing map'
                onlydest  = dest \\ accum

unionTransition :: NFATrans -> [StateNumber] -> Map.Map (Maybe Char) [StateNumber]
unionTransition trans states = Map.delete Nothing . Map.unionsWith union $ maps
    where lookup' trans k = Map.findWithDefault Map.empty k trans
          maps            = map (lookup' trans) states
