
-- module Main ( match ) where

import Hegex.Type
import Hegex.Tree
import Hegex.NFA
import Data.List
import System.Environment

main :: IO ()
main = do
  (str:_) <- getArgs
  print (assemble . buildTree $ str)

-- match :: Pattern -> String -> Bool
-- match pat str = compile pat $ str

-- compile :: Pattern -> DFA
-- compile = convert . interpret . analyze . tokenize

-- interpret :: Tree -> NFA

-- convert :: NFA -> DFA