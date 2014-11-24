
module Main where 

import Hegex
import System.Environment

main :: IO ()
main = do
  (option:pat:str:_) <- getArgs
  case option of
    "-m"      -> print $ isMatch pat str
    "-a"      -> mapM_ putStrLn (matchAll pat str)
