import qualified Data.Map as Map

-- This type is not equipped to deal with NFAs in general--only NFAs that are the 
-- Thomson construction of some regular expression.

data Symbol = Epsilon | Symbol Char deriving (Show, Read, Eq, Ord)

data Nfa = Nfa 
    { accept :: Bool
    , transition :: Map.Map Symbol [Nfa]
    } deriving (Show, Read, Eq)

data NfaConstruction = NfaConstruction
    { initial :: Nfa
    , final :: Nfa
    } deriving (Show, Read, Eq)

emptyState :: Bool -> Nfa
emptyState accept = Nfa { accept = accept
                        , transition = Map.fromList []
                        }

addEdge :: Nfa -> Symbol -> Nfa -> Nfa
addEdge nfa symbol target = Nfa (Map.insertWith (++) symbol [target] (transition nfa))

emptyConstruction :: NfaConstruction
emptyConstruction = NfaConstruction { initial = addEdge (emptyState False) Epsilon final
                                    , final = final
                                    }
    where final = emptyState 

-- Adds a null edge from the final state to the initial state, creates a new final state, and adds
-- null edges from the initial state and old final state to the new final state.
--kleeneStarNfa :: NfaComponent -> Nfa
--kleeneStarNfa nfa = addEdge (final nfa) Epsilon (initial nfa)

-- Adds a null edge between all accepting states in the first NFA and the initial state of the second NFA. 
--connectNfas :: Nfa -> Nfa -> Nfa
--connectNfas nfa1 nfa2 = connectNfasRecursive nfa1 nfa2 []

--connectNfasRecursive :: Nfa -> Nfa -> [Nfa] -> Nfa
--connectNfasRecursive nfa1 nfa2 seen = Nfa { initial = initial nfa1
--                                          , transition 
--                                          }

--postfixRegexToNfa :: String -> Nfa
--postfixRegexToNfa regex = 