
module Hegex.DFA ( enfa2nfa, nfa2dfa, convert, simulate ) where

import           Data.List
import           Data.Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Hegex.Type
import           Control.Monad.Reader

simulate :: DFA -> String -> Bool
simulate (DFA init trans accept) str = loop init str
    where
      loop state []     = Set.member state accept 
      loop state (c:cs) = case Map.lookup (c, state) trans of
                             Just next -> loop next cs
                             Nothing   -> False
                                                                                                 
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
      dfaMakerRes  = runReader (deleteNondetermin dfaMakerInit [nfaInit nfa]) nfa


getAcceptList :: NFA -> DFAMaker -> Accepts
getAcceptList nfa dfaMaker = Set.fromList [ i | (subset, i) <- subsetIndices, isAccepted subset ]
    where
      subsetIndices     = Map.assocs (subsetIndexer dfaMaker)
      isAccepted subset = not . Set.null $ Set.intersection (nfaAccept nfa) subset
                                           

deleteNondetermin :: DFAMaker -> StateSubset -> Reader NFA DFAMaker
deleteNondetermin dfaMaker start =
    do
      translist <- mapReader Map.assocs (unionTransitionFrom start)
      let unsearched tl = foldr filterfunc [] (map snd tl)
          filterfunc subset acc = case lookupSubsetIndex dfaMaker subset of
                                    Just _  -> acc
                                    Nothing -> subset:acc
      dfaMaker' <- connectAdjacent dfaMaker start translist
      foldM deleteNondetermin dfaMaker' (unsearched translist)

connectAdjacent :: DFAMaker -> StateSubset -> [(Maybe Char, StateSubset)] -> Reader NFA DFAMaker
connectAdjacent dfaMaker start translist = do
  let s = fromJust $ lookupSubsetIndex dfaMaker start
  return $ foldl (connectFrom s) dfaMaker translist
      
connectFrom :: StateNumber -> DFAMaker -> (Maybe Char, StateSubset) -> DFAMaker
connectFrom s (DFAMaker sets cnt dfaTrans) (Just c, dest)
    = case Map.lookup (Set.fromList dest) sets of 
        Just n  -> (DFAMaker sets cnt (Map.insert (c, s) n dfaTrans))
        Nothing -> (DFAMaker (Map.insert (Set.fromList dest) (cnt+1) sets)
                             (cnt+1)
                             (Map.insert (c, s) (cnt+1) dfaTrans))

lookupSubsetIndex :: DFAMaker -> [StateNumber] -> Maybe StateNumber
lookupSubsetIndex dfaMaker state = Map.lookup (Set.fromList state) (subsetIndexer dfaMaker)

enfa2nfa :: ENFA -> NFA
enfa2nfa enfa = runReader (deleteEpsilonTrans initnfa (nfaInit enfa)) enfa
    where initnfa = NFA { nfaInit   = nfaInit enfa,
                          nfaTrans  = Map.empty,
                          nfaAccept = Set.empty }

deleteEpsilonTrans :: NFA -> StateNumber -> Reader ENFA NFA
deleteEpsilonTrans nfa start = do
  epstrans <- collectEpsilonTrans start
  accept'  <- updateAccept nfa epstrans start  
  subtrans <- unionTransitionFrom epstrans
  let trans' = if Map.null subtrans
               then (nfaTrans nfa)
               else Map.insert start subtrans (nfaTrans nfa)
  if Map.member start (nfaTrans nfa)
  then return nfa
  else foldM deleteEpsilonTrans (NFA (nfaInit nfa) trans' accept') (concat $ Map.elems subtrans)

updateAccept :: NFA -> StateSubset -> StateNumber -> Reader ENFA Accepts
updateAccept nfa epstrans start = do
  accept <- reader $ head . Set.toList . nfaAccept
  return $ if accept `elem` epstrans
           then Set.insert start (nfaAccept nfa)
           else nfaAccept nfa

collectEpsilonTrans :: StateNumber -> Reader ENFA StateSubset
collectEpsilonTrans start = do
  enfa <- ask
  let loop start' accum
          | onlydest == [] = accum
          | otherwise      = foldr (\x acc -> loop x acc) (accum ++ onlydest) onlydest
          where map'      = Map.findWithDefault Map.empty start' (nfaTrans enfa)
                dest      = Map.findWithDefault [] Nothing map'
                onlydest  = dest \\ accum
  return $ loop start [start]

unionTransitionFrom :: StateSubset -> Reader NFA (Map.Map (Maybe Char) [StateNumber])
unionTransitionFrom states = do
  nfa <- ask
  let lookup' trans k = Map.findWithDefault Map.empty k trans
      maps            = map (lookup' $ nfaTrans nfa) states
  return $ Map.delete Nothing . Map.unionsWith union $ maps
