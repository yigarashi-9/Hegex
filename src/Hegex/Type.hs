
module Hegex.Type where

import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrMsg  = String
type Pattern = String

data Token = Character Char | Union | Star | LParen | RParen deriving(Show, Eq)

data Tree  = TCharacter (Maybe Char)
           | TUnion Tree Tree
           | TStar Tree
           | TConcat Tree Tree deriving(Show, Eq)

type StateNumber = Int
type NFATrans    = Map.Map StateNumber (Map.Map (Maybe Char) [StateNumber])
type Accepts     = Set.Set StateNumber
type Searcher    = (StateNumber, NFATrans)
type Range       = (Int, Int)

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

type SubsetIndexer = Map.Map (Set.Set StateNumber) StateNumber
type StateSubset   = [StateNumber]

data DFAMaker = DFAMaker { subsetIndexer :: SubsetIndexer,
                           dfamCounter   :: StateNumber,
                           dfamTrans     :: DFATrans
                         } deriving(Eq, Show)
