module NfaTests where

import Test.HUnit
import Nfa
import Data.List

makeTest :: Nfa -> String -> Bool -> Test
makeTest nfa string expected = TestCase (assertEqual "test" expected (nfa `nfaAcceptsString` string))

makeTests :: Nfa -> [(String, Bool)] -> [Test]
makeTests nfa = map (\(x, b) -> makeTest nfa x b)

literalA = literalConstruction (Symbol 'A')
literalB = literalConstruction (Symbol 'B')
literalC = literalConstruction (Symbol 'C')

-- 'AB'
complex1 = concatenateNfas literalA literalB
-- 'B|C'
complex2 = unionNfas literalB literalC
-- 'AB(B|C)'
complex3 = concatenateNfas complex1 complex2
-- '(AB(B|C))*'
complex4 = kleeneStarNfa complex3
-- '(AB(B|C))*(B|C)
complex5 = concatenateNfas complex4 complex2
-- '((AB(B|C))*(B|C))|C
complex6 = unionNfas literalC complex5

literalATests = makeTests literalA 
    [ ("A", True)
    , ("B", False)
    ]

complex1Tests = makeTests complex1
    [ ("AB", True)
    , ("BA", False)
    , ("A", False)
    , ("B", False)
    , ("ABA", False)
    , ("", False)
    , ("ABBAB", False)
    ]

complex2Tests = makeTests complex2
    [ ("A", False)
    , ("B", True)
    , ("C", True)
    , ("", False)
    , ("AB", False)
    , ("BA", False)
    ]

complex4Tests = makeTests complex4
    [ ("", True)
    , ("ABC", True)
    , ("ABA", False)
    , ("ABCABCABCABCABCABCABCABC", True)
    , ("ABCABBABCABB", True)
    , ("ABCABBABCABA", False) 
    ]

complex6Tests = makeTests complex6
    [ ("", False)
    , ("C", True)
    , ("ABCB", True)
    , ("B", True)
    , ("ABBABBC", True)
    , ("ABBABB", False)
    ]

tests = TestList 
    (  literalATests 
    ++ complex1Tests 
    ++ complex2Tests 
    ++ complex4Tests
    ++ complex6Tests
    )