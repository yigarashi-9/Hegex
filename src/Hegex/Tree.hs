
module Hegex.Tree ( buildTree ) where

import Hegex.Type

type Pattern = String

data Token = Character Char | Union | Star | LParen | RParen deriving(Show, Eq)

buildTree :: Pattern -> Tree
buildTree = analyze . tokenize

tokenize :: Pattern -> [Token]
tokenize []        = []
tokenize ('\\':cs) = (Character $ head cs):(tokenize $ tail cs)
tokenize (c:cs)    = token:(tokenize cs)
    where token
              | c == '|'  = Union
              | c == '*'  = Star
              | c == '('  = LParen
              | c == ')'  = RParen
              | otherwise = Character c

badPattern :: [Token] -> Bool
badPattern []       = False
badPattern (Star:_) = True
badPattern (_:ts)   = loop ts
    where
      loop []           = False
      loop (_:[])       = False
      loop (t1:t2:rest)
          | (t1, t2) `elem` badpat = True
          | otherwise              = loop (t2:rest)
          where badpat = [(Star, Star), (Union, Star), (Union, Union), (LParen, Star)]
          

badParenthesis :: [Token] -> Bool
badParenthesis token = loop token 0
    where
      loop :: [Token] -> Int -> Bool
      loop []     n = n /= 0
      loop (t:ts) n
          | n < 0       = True
          | t == LParen = loop ts (n+1)
          | t == RParen = loop ts (n-1)
          | otherwise   = loop ts n

-- Grammer rule quoted from 'http://codezine.jp/article/detail/3158?p=2'
-- (A) expression -> subexpr EOF
-- (B) subexpr -> seq '|' subexpr | seq
-- (C) seq -> subseq | ''
-- (D) subseq -> star subseq | star
-- (E) star -> factor '*' | factor
-- (F) factor -> '(' subexpr ')' | CHARACTER

analyze :: [Token] -> Tree
analyze ts 
    | badPattern ts     = error "Syntax error : Bad Pattern."
    | badParenthesis ts = error "Syntax error : Bad Parenthesis."
    | otherwise         = tree
    where (tree, _)     = subexpression ts
                    
subexpression :: [Token] -> (Tree, [Token])
subexpression ts = loop sequ rest
    where (sequ, rest) = parseSubexp ts
          loop tree tokens
               | checkHead tokens Union  = let (sequl, restl) = parseSubexp (tail tokens)
                                           in loop (TUnion tree sequl) restl
               | otherwise              = (tree, tokens)

parseSubexp :: [Token] -> (Tree, [Token])
parseSubexp ts = if checkHead ts LParen || checkHeadChar ts
                 then subsequence ts
                 else (TCharacter Nothing, ts)

subsequence :: [Token] -> (Tree, [Token])
subsequence ts = loop starred rest
    where (starred, rest) = star ts
          loop tree tokens
              | checkHead tokens LParen || checkHeadChar tokens = loop (TConcat tree starredl) restl
              | otherwise =  (tree, tokens)
              where (starredl, restl) = star tokens

star :: [Token] -> (Tree, [Token])
star ts
    | checkHead rest Star   = (TStar fac, tail rest)
    | otherwise             = (fac, rest)
    where (fac, rest) = factor ts

factor :: [Token] -> (Tree, [Token])
factor ts
    | checkHead ts LParen = (subexp, tail tokens)
    | otherwise           = (TCharacter (getHeadChar ts), tail ts)
    where (subexp, tokens) = subexpression (tail ts)

getHeadChar :: [Token] -> Maybe Char
getHeadChar ((Character c):_) = Just c
getHeadChar _                 = Nothing

checkHead :: [Token] -> Token -> Bool
checkHead []    _     = False
checkHead (t:_) token = t == token

checkHeadChar :: [Token] -> Bool
checkHeadChar ((Character _):_) = True
checkHeadChar _                 = False
