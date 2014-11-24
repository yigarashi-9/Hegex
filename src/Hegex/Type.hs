
module Hegex.Type where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Tree  = TCharacter (Maybe Char)
           | TUnion Tree Tree
           | TStar Tree
           | TConcat Tree Tree deriving(Show, Eq)

type StateNumber = Int
type StateSubset = Set.Set StateNumber
type Accepts     = StateSubset
type NFATrans    = Map.Map StateNumber (Map.Map (Maybe Char) StateSubset)

data NFA = NFA { nfaInit   :: StateNumber,
                 nfaTrans  :: NFATrans,
                 nfaAccept :: Accepts
               } deriving(Eq, Show)

type ENFA = NFA
type ENFATrans = NFATrans

type DFATrans = Map.Map (Char, StateNumber) StateNumber
data DFA = DFA { dfaInit   :: StateNumber,
                 dfaTrans  :: DFATrans,
                 dfaAccept :: Accepts
               } deriving(Eq, Show)
