
import Hegex.Type
import Hegex.Tree
import Hegex.NFA
import Hegex.DFA
import System.Environment

main :: IO ()
main = do
  (str1:str2:_) <- getArgs
  print (simulate (convert . assemble . buildTree $ str1) str2)

makeTree :: String -> Tree
makeTree = buildTree

makeENFA :: String -> ENFA
makeENFA = assemble . buildTree
         
makeNFA :: String -> NFA
makeNFA = enfa2nfa . assemble . buildTree
         
makeDFA :: String -> DFA
makeDFA = convert . assemble . buildTree
