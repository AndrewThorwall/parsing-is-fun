module RegexTests where

import Test.HUnit
import Regex

makeTest :: Regex -> String -> Bool -> Test
makeTest regex string expected = TestCase (assertEqual "test" expected (regex `regexAccepts` string))

makeTests :: Regex -> [(String, Bool)] -> [Test]
makeTests regex = map (\(x, b) -> makeTest regex x b)

regex1 = buildRegex "ABCDE"
regex2 = buildRegex "(A|B)*"
regex3 = buildRegex "(ABC*)|(A*BC)|(AB*C)"

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

tests = TestList
    (  regex1Tests
    ++ regex2Tests
    ++ regex3Tests
    )
