module Postfix where

isOperator :: Char -> Bool
isOperator x = elem x ['*', '+', '.', '|']

isLiteral :: Char -> Bool
isLiteral x
    | (not $ isOperator x) && (not $ x `elem` ['(', ')']) = True
    | otherwise = False

-- We define precedence for parentheses to make infixToPostfix cleaner.
precedence :: Char -> Integer
precedence '*' = 3
precedence '+' = 3
precedence '.' = 2
precedence '|' = 1
precedence '(' = 0
precedence ')' = 0
precedence x = error $ [x] ++ " is not an operator or parenthesis."

popAndPushWhile :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
popAndPushWhile pred xs ys = (xs ++ (takeWhile pred $ reverse ys), 
                              reverse (dropWhile pred $ reverse ys))

infixToPostfix :: String -> String
infixToPostfix xs = infixToPostfixRec xs [] []

-- Shunting-yard algorithm. We assume all operators are left-associative.
infixToPostfixRec :: String -> [Char] -> [Char] -> String 
infixToPostfixRec [] out ops = out ++ (reverse ops)
infixToPostfixRec (x:xs) out ops
    | isOperator x = let (out', ops') = popAndPushWhile (\y -> precedence y >= precedence x) out ops in 
        infixToPostfixRec xs out' (ops' ++ [x])
    | x == '(' = infixToPostfixRec xs out (ops ++ [x])
    | x == ')' = let (out', ops') = popAndPushWhile (/= '(') out ops in
        infixToPostfixRec xs out' (init ops')
    | otherwise = infixToPostfixRec xs (out ++ [x]) ops

