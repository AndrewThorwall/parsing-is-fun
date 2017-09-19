module Regex where

import Nfa
import Postfix

data Regex = Regex Nfa

decideOperator :: Char -> Char -> Bool
decideOperator x y
    | (isLiteral x || x == ')' || x == '*') && (isLiteral y || y == '(') = True
    | otherwise = False

intersperseOperator :: Char -> String -> String
intersperseOperator = intersperseOperatorRec []

intersperseOperatorRec :: String -> Char -> String -> String
intersperseOperatorRec result c (x1:x2:xs) 
    | decideOperator x1 x2 = intersperseOperatorRec ('.':x1:result) c (x2:xs)
    | otherwise = intersperseOperatorRec (x1:result) c (x2:xs)
intersperseOperatorRec result c (x1:[]) = reverse $ x1:result

regexToInfix :: String -> String
regexToInfix r = intersperseOperator '.' r

buildRegex :: String -> Regex
buildRegex str = Regex $ (postfixToNfa . infixToPostfix . regexToInfix) str

regexAccepts :: Regex -> String -> Bool
regexAccepts (Regex nfa) s = nfaAcceptsString nfa s