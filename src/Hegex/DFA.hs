
module Hegex.DFA ( enfa2nfa, nfa2dfa, convert, simulate ) where

import           Data.Maybe
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Hegex.Type
import           Control.Monad.Reader

simulate :: DFA -> String -> Bool
simulate (DFA dinit dtrans daccept) str = loop dinit str
    where
      loop state []     = Set.member state daccept 
      loop state (c:cs) = case Map.lookup (c, state) dtrans of
                             Just next -> loop next cs
                             Nothing   -> False
                                                                                                 
convert :: ENFA -> DFA
convert = nfa2dfa . enfa2nfa

-- In NFA, transition destination by some character is a subset of StateNumber.
-- To configure DFA, index all necessary subsets and build map of new indices.
type SubsetIndexer = Map.Map StateSubset StateNumber

data DFAMaker = DFAMaker { subsetIndexer :: SubsetIndexer,
                           dfamCounter   :: StateNumber,
                           dfamTrans     :: DFATrans
                         } deriving(Eq, Show)
          
nfa2dfa :: NFA -> DFA
nfa2dfa nfa = DFA { dfaInit   = 0,
                    dfaTrans  = dfamTrans dfaMakerRes,
                    dfaAccept = getAcceptList nfa dfaMakerRes }
    where
      dfaMakerInit = DFAMaker { subsetIndexer = Map.singleton (Set.singleton $ nfaInit nfa) 0,
                                dfamCounter   = 0,
                                dfamTrans     = Map.empty }
      dfaMakerRes  = runReader (deleteNondetermin dfaMakerInit (Set.singleton $ nfaInit nfa)) nfa

getAcceptList :: NFA -> DFAMaker -> Accepts
getAcceptList nfa dfaMaker = Set.fromList [ i | (subset, i) <- subsetIndices, isAccepted subset ]
    where
      subsetIndices     = Map.assocs (subsetIndexer dfaMaker)
      isAccepted subset = not . Set.null $ Set.intersection (nfaAccept nfa) subset
                                           

deleteNondetermin :: DFAMaker -> StateSubset -> Reader NFA DFAMaker
deleteNondetermin dfaMaker start = do
  translist <- mapReader Map.assocs (unionTransitionsFrom start)
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
connectFrom _ (DFAMaker _ _ _) (Nothing, _) = undefined -- never occurs
connectFrom s (DFAMaker sets cnt dtrans) (Just c, dest)
    = case Map.lookup dest sets of 
        Just n  -> (DFAMaker sets cnt (Map.insert (c, s) n dtrans))
        Nothing -> (DFAMaker (Map.insert dest (cnt+1) sets)
                             (cnt+1)
                             (Map.insert (c, s) (cnt+1) dtrans))

lookupSubsetIndex :: DFAMaker -> StateSubset -> Maybe StateNumber
lookupSubsetIndex dfaMaker state = Map.lookup state (subsetIndexer dfaMaker)


-- First, epsilon transitions from a start point of ENFA are collected.
-- Second, those transitions are unioned. This unioned transition is a
-- new transition of NFA. Last, run this algorithm with using the
-- collected points which are not visited as a new start point.
enfa2nfa :: ENFA -> NFA
enfa2nfa enfa = runReader (deleteEpsilonTrans initnfa (nfaInit enfa)) enfa
    where initnfa = NFA { nfaInit   = nfaInit enfa,
                          nfaTrans  = Map.empty,
                          nfaAccept = Set.empty }

deleteEpsilonTrans :: NFA -> StateNumber -> Reader ENFA NFA
deleteEpsilonTrans nfa start
    | Map.member start (nfaTrans nfa) = return nfa -- equals to alreadyVisit?
    | otherwise = do
  epsilondests <- collectEpsilonTrans start
  unionedtrans <- unionTransitionsFrom epsilondests
  accept'      <- updateAccept nfa start epsilondests
  let trans' = Map.insert start unionedtrans (nfaTrans nfa)
  foldM deleteEpsilonTrans
            (NFA (nfaInit nfa) trans' accept')
            (Set.toList . Set.unions $ Map.elems unionedtrans)

-- If epsilon transition destinations contain accept states,
-- the start state will be accept state in NFA.
updateAccept :: NFA -> StateNumber -> StateSubset -> Reader ENFA Accepts
updateAccept nfa start epsdests = do
  accept <- reader $ head . Set.toList . nfaAccept
  return $ if Set.member accept epsdests
           then Set.insert start (nfaAccept nfa)
           else nfaAccept nfa

collectEpsilonTrans :: StateNumber -> Reader ENFA StateSubset
collectEpsilonTrans start = collectLoop (Set.singleton start) start

collectLoop :: StateSubset -> StateNumber -> Reader ENFA StateSubset
collectLoop acc start = do
  dest <- findEpsTransFrom start
  let unsearched = Set.difference dest acc
  foldM collectLoop (Set.union acc unsearched) (Set.toList unsearched)

findEpsTransFrom :: StateNumber -> Reader ENFA StateSubset
findEpsTransFrom start = do
  enfa <- ask
  return $ Map.findWithDefault Set.empty Nothing .
           Map.findWithDefault Map.empty start  $ nfaTrans enfa
              
unionTransitionsFrom :: StateSubset -> Reader NFA (Map.Map (Maybe Char) StateSubset)
unionTransitionsFrom states = do
  nfa <- ask
  let unionTransMap s acc = Map.unionWith Set.union acc . lookUp (nfaTrans nfa) $ s
      lookUp trans k      = Map.findWithDefault Map.empty k trans
  return $ Map.delete Nothing $ Set.foldr unionTransMap Map.empty states
