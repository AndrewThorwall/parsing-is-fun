module Nfa where

import Data.List
import qualified Data.Map as Map

-- This type is not equipped to deal with NFAs in general--only NFAs that are the 
-- Thomson construction of some regular expression.

data Symbol = Epsilon 
            | Symbol Char deriving (Show, Read, Eq, Ord)

data NfaState = NfaState Integer deriving (Show, Read, Eq, Ord)

unNfaState :: NfaState -> Integer
unNfaState (NfaState x) = x

shiftNfaState :: Integer -> NfaState -> NfaState
shiftNfaState n (NfaState s) = NfaState $ s + n

data NfaTransition = NfaTransition (Map.Map Symbol [NfaState]) deriving (Show, Read, Eq)

unNfaTransition :: NfaTransition -> Map.Map Symbol [NfaState]
unNfaTransition (NfaTransition m) = m

emptyNfaTransition :: NfaTransition
emptyNfaTransition = NfaTransition (Map.fromList [])

addEdgeToTransition :: Symbol -> NfaState -> NfaTransition -> NfaTransition
addEdgeToTransition symbol state (NfaTransition n) = NfaTransition $
    Map.insertWith (union) symbol [state] n

shiftNfaTransition :: Integer -> NfaTransition -> NfaTransition
shiftNfaTransition n (NfaTransition t) = NfaTransition $
    Map.map (map (\(NfaState x) -> NfaState $ x + n)) t

data Nfa = Nfa 
    { states :: Map.Map NfaState NfaTransition
    , initial :: NfaState
    , final :: NfaState
    } deriving (Show, Read, Eq)

getNewState :: Nfa -> NfaState
getNewState (Nfa { states = s }) 
    | length (Map.keys s) == 0 = NfaState 0
    | otherwise                = NfaState ((unNfaState $ maximum (Map.keys s)) + 1)

addState :: NfaState -> Nfa -> Nfa
addState s (Nfa { states = n, initial = i, final = f }) = Nfa 
    { states = Map.insert s emptyNfaTransition n
    , initial = i
    , final = f
    } 

addEdge :: Symbol -> NfaState -> NfaState -> Nfa -> Nfa
addEdge symbol s1 s2 (Nfa { initial = i, states = n, final = f }) = Nfa 
    { states = Map.adjust (addEdgeToTransition symbol s2) s1 n 
    , initial = i
    , final = f
    }

setInitial :: NfaState -> Nfa -> Nfa
setInitial state nfa = Nfa 
    { states = states nfa
    , initial = state
    , final = final nfa
    }

setFinal :: NfaState -> Nfa -> Nfa
setFinal state nfa = Nfa 
    { states = states nfa
    , initial = initial nfa
    , final = state
    }

-- Given a symbol, creates a NFA consisting of an initial state and a final state,
-- and a transition from the initial state to the final state for that symbol.
literalConstruction :: Symbol -> Nfa
literalConstruction s = Nfa
    { states = Map.fromList [(NfaState 0, 
                              NfaTransition $ Map.fromList [(s,[NfaState 1])]), 
                             (NfaState 1, 
                              NfaTransition $ Map.fromList [])]
    , initial = NfaState 0
    , final = NfaState 1
    }

shiftNfa :: Integer -> Nfa -> Nfa
shiftNfa n nfa = Nfa
    { states = ((Map.mapKeys (shiftNfaState n)) . (Map.map (shiftNfaTransition n))) (states nfa)
    , initial = NfaState $ unNfaState (initial nfa) + n
    , final = NfaState $ unNfaState (final nfa) + n
    }

combineNfas :: Nfa -> Nfa -> (Nfa, Integer)
combineNfas nfa1 nfa2 = (nfa, shift)
    where nfa = Nfa 
              { states = Map.union (states nfa1) (states nfa2')
              , initial = initial nfa1
              , final = final nfa1
              }
          nfa2' = shiftNfa shift nfa2
          shift = unNfaState (getNewState nfa1)

-- Given two NFAs, creates a new NFA by adding an empty edge from the final state of the first
-- NFA to the initial state of the second NFA. State keys will be updated to avoid collisions.
concatenateNfas :: Nfa -> Nfa -> Nfa
concatenateNfas nfa1 nfa2 = addEdge Epsilon (final nfa1) shiftedInitial adjusted
    where (combined, shift) = combineNfas nfa1 nfa2
          shiftedInitial = shiftNfaState shift $ initial nfa2
          shiftedFinal = shiftNfaState shift $ final nfa2
          adjusted = Nfa
              { states = states combined
              , initial = initial nfa1 
              , final = shiftedFinal 
              }

unionNfas :: Nfa -> Nfa -> Nfa
unionNfas nfa1 nfa2 = ( (addEdge Epsilon newInitial (initial nfa1))
                      . (addEdge Epsilon newInitial shiftedInitial)
                      . (addEdge Epsilon (final nfa1) newFinal)
                      . (addEdge Epsilon shiftedFinal newFinal)
                      . (setInitial newInitial)
                      . (setFinal newFinal) ) combinedComplete
    where (combined, shift) = combineNfas nfa1 nfa2
          newInitial = getNewState combined
          shiftedInitial = shiftNfaState shift $ initial nfa2
          shiftedFinal = shiftNfaState shift $ final nfa2
          combinedWithInitial = addState newInitial combined
          newFinal = getNewState combinedWithInitial
          combinedComplete = addState newFinal combinedWithInitial

kleeneStarNfa :: Nfa -> Nfa
kleeneStarNfa nfa = ( (addEdge Epsilon (final nfa) (initial nfa))
                    . (addEdge Epsilon newInitial (initial nfa))
                    . (addEdge Epsilon (final nfa) newFinal)
                    . (addEdge Epsilon newInitial newFinal) 
                    . (setInitial newInitial)
                    . (setFinal newFinal) ) withFinal
    where newInitial = getNewState nfa
          withInitial = addState newInitial nfa
          newFinal = getNewState withInitial
          withFinal = addState newFinal withInitial

-- Performs an NFA step WITHOUT following empty edges (unless symbol is Epsilon). 
oneStepNfa :: Nfa -> Symbol -> NfaState -> [NfaState]
oneStepNfa nfa symbol state 
    | Map.member symbol t = t Map.! symbol
    | otherwise = []
    where t = unNfaTransition $ (states nfa) Map.! state

oneEpsilonStepNfa :: Nfa -> NfaState -> [NfaState]
oneEpsilonStepNfa nfa state = union [state] (oneStepNfa nfa Epsilon state)


closure :: (Eq a) => [a] -> (a -> [a]) -> [a]
closure state transition = closureRec state transition Nothing

closureRec :: (Eq a) => [a] -> (a -> [a]) -> Maybe [a] -> [a]
closureRec state transition Nothing = closureRec (nub $ state >>= transition) transition (Just state)
closureRec state transition (Just lastState)
    | state == lastState = state
    | otherwise = closureRec (nub $ state >>= transition) transition (Just state)

epsilonClosure :: Nfa -> NfaState -> [NfaState]
epsilonClosure nfa state = closure [state] (oneEpsilonStepNfa nfa)

stepNfa :: Nfa -> Symbol -> NfaState -> [NfaState]
stepNfa nfa symbol state = nub $ (epsilonClosure nfa state) >>= (oneStepNfa nfa symbol) >>= (epsilonClosure nfa)

stepSetNfa :: Nfa -> [NfaState] -> Symbol -> [NfaState]
stepSetNfa nfa states symbol = nub $ states >>= stepNfa nfa symbol

accepts :: Nfa -> [Symbol] -> Bool
accepts nfa symbols = (final nfa) `elem` (foldl (stepSetNfa nfa) (epsilonClosure nfa $ initial nfa) symbols)

acceptsString :: Nfa -> String -> Bool
acceptsString nfa str = accepts nfa (map Symbol str)

