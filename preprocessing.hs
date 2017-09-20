-- The first step in building a regular expression is converting it from standard regex notation
-- (e.g. "a*b(f\+r)") to a notation convient for building our NFA. The process is:
--   1. identify characters as either literal (Literal c) or reserved (Reserved c). The only interesting 
--      effect of this is that escaped version of otherwise reserved characters are identified as literals.
--   2. convert the expression to infix notation by inserting a concatenation operator ('&') where necessary.
--   3. convert the infix expression to postfix notation with the Shunting yard algorithm.

module Preprocessing where

data RegexChar = Literal Char
               | Reserved Char deriving (Show, Read, Eq)

unRegexChar :: RegexChar -> Char
unRegexChar (Literal c) = c
unRegexChar (Reserved c) = c

toRegexChar :: Char -> RegexChar
toRegexChar c
    | isCharOperator c = Reserved c
    | c == ')' || c == '(' || c == '.' = Reserved c
    | otherwise = Literal c

toRegexString :: String -> [RegexChar]
toRegexString (x1:x2:xs)
    | x1 == '\\' = (Literal x2):(toRegexString xs)
    | otherwise = (toRegexChar x1):(toRegexString $ x2:xs)
toRegexString (x:xs) = (toRegexChar x):(toRegexString xs)
toRegexString [] = []

isCharOperator :: Char -> Bool
isCharOperator c = elem c ['*', '+', '&', '|', '?']

isOperator :: RegexChar -> Bool
isOperator (Literal c) = False
isOperator (Reserved c) = elem c ['*', '+', '&', '|', '?']

isLiteral :: RegexChar -> Bool
isLiteral (Reserved _) = False
isLiteral (Literal _) = True

canEndExpression :: RegexChar -> Bool
canEndExpression (Reserved c) = c `elem` [')', '*', '+', '?', '.']
canEndExpression (Literal c) = True

canStartExpression :: RegexChar -> Bool
canStartExpression (Reserved c) = c `elem` ['(', '.']
canStartExpression (Literal c) = True

isReserved :: RegexChar -> Bool
isReserved = not . isLiteral

-- We define precedence for parentheses to make infixToPostfix cleaner.
precedence :: RegexChar -> Integer
precedence (Reserved '*') = 3
precedence (Reserved '+') = 3
precedence (Reserved '?') = 3
precedence (Reserved '&') = 2
precedence (Reserved '|') = 1
precedence (Reserved '(') = 0
precedence (Reserved ')') = 0
precedence x = error $ (show x) ++ " is not an operator or parenthesis."

popAndPushWhile :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
popAndPushWhile pred xs ys = (xs ++ (takeWhile pred $ reverse ys), 
                              reverse (dropWhile pred $ reverse ys))

infixToPostfix :: [RegexChar] -> [RegexChar]
infixToPostfix xs = infixToPostfixRec xs [] []

-- Shunting-yard algorithm. We assume all operators are left-associative.
infixToPostfixRec :: [RegexChar] -> [RegexChar] -> [RegexChar] -> [RegexChar] 
infixToPostfixRec [] out ops = out ++ (reverse ops)
infixToPostfixRec (x:xs) out ops
    | isOperator x = let (out', ops') = popAndPushWhile (\y -> precedence y >= precedence x) out ops in 
        infixToPostfixRec xs out' (ops' ++ [x])
    | x == (Reserved '(') = infixToPostfixRec xs out (ops ++ [x])
    | x == (Reserved ')') = let (out', ops') = popAndPushWhile (/= (Reserved '(')) out ops in
        infixToPostfixRec xs out' (init ops')
    | otherwise = infixToPostfixRec xs (out ++ [x]) ops

decideOperator :: RegexChar -> RegexChar -> Bool
decideOperator x y
    | (canEndExpression x) && (canStartExpression y) = True
    | otherwise = False

intersperseOperator :: RegexChar -> [RegexChar] -> [RegexChar]
intersperseOperator = intersperseOperatorRec []

intersperseOperatorRec :: [RegexChar] -> RegexChar -> [RegexChar] -> [RegexChar]
intersperseOperatorRec result c (x1:x2:xs) 
    | decideOperator x1 x2 = intersperseOperatorRec (c:x1:result) c (x2:xs)
    | otherwise = intersperseOperatorRec (x1:result) c (x2:xs)
intersperseOperatorRec result c (x1:[]) = reverse $ x1:result

regexToInfix :: [RegexChar] -> [RegexChar]
regexToInfix r = intersperseOperator (Reserved '&') r