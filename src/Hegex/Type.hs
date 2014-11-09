
module Hegex.Type where

import qualified Data.Map as Map

type ErrMsg  = String
type Pattern = String

data Token = Character Char | Union | Star | LParen | RParen deriving(Show, Eq)

data Tree  = TCharacter (Maybe Char)
           | TUnion Tree Tree
           | TStar Tree
           | TConcat Tree Tree deriving(Show, Eq)

type StateNumber = Int
type TransitionN = Map.Map StateNumber (Map.Map (Maybe Char) [StateNumber])
type Searcher    = (StateNumber, TransitionN)
type Range       = (Int, Int)

data NFA = NFA { initialN :: StateNumber,
                 transitionN :: TransitionN,
                 acceptN :: StateNumber
               } deriving(Eq, Show)

type TransitionD = Map.Map (Char, StateNumber) StateNumber

data DFA = DFA { initialD :: StateNumber,
                 transitionD :: TransitionD,
                 acceptD :: [StateNumber]
               } deriving(Eq, Show)
