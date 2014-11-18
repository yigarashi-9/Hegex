
module Hegex.DFA ( enfa2nfa, nfa2dfa, convert, simulate ) where

import           Data.List
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Hegex.Type

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
                                           

deleteNondetermin :: NFA -> DFAMaker -> StateSubset -> DFAMaker
deleteNondetermin nfa dfaMaker start
    = foldl (deleteNondetermin nfa) dfaMaker' (unsearched translist)
    where
      translist    = Map.assocs $ unionTransition nfa start
      dfaMaker'    = connectAdjacent translist dfaMaker start
      unsearched l = foldr filterfunc [] l
      filterfunc (_, list) acc = if lookupFromlist dfaMaker list == notExist
                                 then (list:acc)
                                 else acc

connectAdjacent :: [(Maybe Char, StateSubset)] -> DFAMaker -> StateSubset -> DFAMaker
connectAdjacent translist dfaMaker start = foldl step dfaMaker translist
    where
      startNum  = lookupFromlist dfaMaker start
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
enfa2nfa nfa = deleteEpsilonTrans nfa (NFA (nfaInit nfa) Map.empty Set.empty) (nfaInit nfa)

deleteEpsilonTrans :: ENFA -> NFA -> StateNumber -> NFA
deleteEpsilonTrans enfa nfa start
    = if Map.member start (nfaTrans nfa)
      then nfa
      else foldl (deleteEpsilonTrans enfa) (NFA (nfaInit nfa) transAcc' acceptAcc') dest
    where etrans     = collectEpsilonTrans enfa start
          accept     = head $ Set.toList (nfaAccept enfa)
          subtrans   = unionTransition enfa etrans
          dest       = concat $ Map.elems subtrans
          acceptAcc' = if accept `elem` etrans
                       then Set.insert start (nfaAccept nfa)
                       else (nfaAccept nfa)
          transAcc'  = if Map.null subtrans
                       then (nfaTrans nfa)
                       else Map.insert start subtrans (nfaTrans nfa)


collectEpsilonTrans :: ENFA -> StateNumber -> [StateNumber]
collectEpsilonTrans enfa start = loop start [start]
    where 
      loop start' accum
          | onlydest == [] = accum
          | otherwise      = foldr (\x acc -> loop x acc) (accum ++ onlydest) onlydest
          where map'      = Map.findWithDefault Map.empty start' (nfaTrans enfa)
                dest      = Map.findWithDefault [] Nothing map'
                onlydest  = dest \\ accum

unionTransition :: NFA -> [StateNumber] -> Map.Map (Maybe Char) [StateNumber]
unionTransition nfa states = Map.delete Nothing . Map.unionsWith union $ maps
    where lookup' trans k = Map.findWithDefault Map.empty k trans
          maps            = map (lookup' $ nfaTrans nfa) states
