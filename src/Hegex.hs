
module Hegex where 

import Hegex.Type
import Hegex.Tree
import Hegex.NFA
import Hegex.DFA
import Data.List

makeTree :: Pattern -> Tree
makeTree = buildTree

makeENFA :: Pattern -> ENFA
makeENFA = assemble . makeTree
         
makeNFA :: Pattern -> NFA
makeNFA = enfa2nfa . makeENFA
         
makeDFA :: Pattern -> DFA
makeDFA = nfa2dfa . makeNFA

getMatcher :: Pattern -> (String -> Bool)
getMatcher = simulate . makeDFA

isMatch :: Pattern -> String -> Bool
isMatch pat str = getMatcher pat $ str

matchAll :: Pattern -> String -> [String]
matchAll pat str = foldr match [] (concat . map tail . map inits . init . tails $ str)
    where
      matcher = getMatcher pat
      match s acc
          | matcher s = s:acc
          | otherwise = acc

-- Correctness of your nfa is not checked.
-- Please use this with care.
getMatcherWithNFA :: NFA -> (String -> Bool)
getMatcherWithNFA = simulate . convert
