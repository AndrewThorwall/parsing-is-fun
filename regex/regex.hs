module Regex where

import Nfa
import Preprocessing

data Regex = Regex Nfa deriving (Show, Read, Eq)

buildRegex :: String -> Regex
buildRegex str = Regex $ (postfixToNfa . infixToPostfix . regexToInfix . toRegexString) str

regexAccepts :: Regex -> String -> Bool
regexAccepts (Regex nfa) s = nfaAcceptsString nfa s

regexMatches :: Regex -> String -> Maybe String
regexMatches (Regex nfa) s = nfaMatchesString nfa s