
module Main where

import Hegex
import System.Environment

main :: IO ()
main = do
  (option:rest) <- getArgs
  run option rest

run :: String -> [String] -> IO ()
run "-m" (pat:str:[]) = print $ isMatch pat str
run "-a" (pat:str:[]) = mapM_ putStrLn (matchAll pat str)
run "-h" _            = mapM_ print
                        [" -m pattern string : check whether string matches the pattern",
                         " -a pattern string : display all substrings matching the pattern"]
run _ _               = print "invalid args. use -h to see the usage."
