module RegexTests where

import Test.HUnit
import Regex

makeTest :: Regex -> String -> Bool -> Test
makeTest regex string expected = TestCase (assertEqual string expected (regex `regexAccepts` string))

makeTests :: Regex -> [(String, Bool)] -> [Test]
makeTests regex = map (\(x, b) -> makeTest regex x b)

regex1 = buildRegex "ABCDE"
regex2 = buildRegex "(A|B)*"
regex3 = buildRegex "(ABC*)|(A*BC)|(AB*C)"
regex4 = buildRegex "(A|B)?(C|D)+(E|F)*"
regex5 = buildRegex ".*-(wow|okay)-.*"
regex6 = buildRegex ".*.+.*o.*.+.?.*"

regex1Tests = makeTests regex1
    [ ("ABCDE", True)
    , ("ABCD", False)
    , ("ABCDEF", False)
    , ("", False)
    , ("AAAAA", False)
    ]

regex2Tests = makeTests regex2
    [ ("", True)
    , ("A", True)
    , ("B", True)
    , ("ABABABA", True)
    , ("C", False)
    , ("ABABC", False)
    ]

regex3Tests = makeTests regex3
    [ ("ABCCCCCCCC", True)
    , ("AB", True)
    , ("ABBBBBBBBBC", True)
    , ("AC", True)
    , ("AAAAAAAAAAABC", True)
    , ("BC", True)
    , ("ABC", True)
    , ("AABBCC", False)
    , ("", False)
    , ("ABCABC", False)
    ]

regex4Tests = makeTests regex4
    [ ("ACE", True)
    , ("AC", True)
    , ("DF", True)
    , ("CE", True)
    , ("AF", False)
    , ("A", False)
    , ("ABCE", False)
    , ("ACCEEEFE", True)
    ]

regex5Tests = makeTests regex5
    [ ("wow-wow-wow", True)
    , ("-okay-", True)
    , ("1-wow-2", True)
    , ("@-okay-", True)
    , ("123--wow---", True)
    , ("wow-okay", False)
    , ("wow-wo-wow", False)
    , ("okay-okay -okay", False)
    ]

regex6Tests = makeTests regex6
    [ ("wow", True)
    , (" o ", True)
    , ("     o     ", True)
    , ("o", False)
    , ("o ", False)
    , (" o", False)
    ]

tests = TestList
    (  regex1Tests
    ++ regex2Tests
    ++ regex3Tests
    ++ regex4Tests
    ++ regex5Tests
    ++ regex6Tests
    )
