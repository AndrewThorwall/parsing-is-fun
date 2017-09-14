import Test.HUnit
import Postfix

popAndPushTest1 = TestCase (assertEqual "Test two empty lists"
                                        (popAndPushWhile (== 3) [] [])
                                        ([], []))

popAndPushTest2 = TestCase (assertEqual "Test second list is empty"
                                        (popAndPushWhile (== 3) [1, 2, 3] [])
                                        ([1, 2, 3], []))
                              
popAndPushTest3 = TestCase (assertEqual "Test first list is empty"
                                        (popAndPushWhile (== 3) [] [1, 2, 3])
                                        ([3], [1, 2]))      

popAndPushTest4 = TestCase (assertEqual "Test both lists are nonempty"
                                        (popAndPushWhile (> 2) [1, 2, 3, 4, 5] [1, 2, 3, 4, 5])
                                        ([1, 2, 3, 4, 5, 5, 4, 3], [1, 2]))

popAndPushTest5 = TestCase (assertEqual "Test predicate matches none"
                                        (popAndPushWhile (> 10) [1, 2, 3, 4, 5] [1, 2, 3, 4, 5])
                                        ([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]))

popAndPushTest6 = TestCase (assertEqual "Test predicate matches all"
                                        (popAndPushWhile (< 10) [1, 2, 3, 4, 5] [1, 2, 3, 4, 5])
                                        ([1, 2, 3, 4, 5, 5, 4, 3, 2, 1], []))

infixToPostfixTest1 = TestCase (assertEqual "Test empty expression"
                                            (infixToPostfix "")
                                            "")

infixToPostfixTest2 = TestCase (assertEqual "Test simple expression"
                                            (infixToPostfix "1.2")
                                            "12.")    
                                            
infixToPostfixTest3 = TestCase (assertEqual "Test left-associativity"
                                            (infixToPostfix "1.2.3")
                                            "12.3.")

infixToPostfixTest4 = TestCase (assertEqual "Test operator precedence"
                                            (infixToPostfix "1|2.3")
                                            "123.|")
infixToPostfixTest5 = TestCase (assertEqual "Complex case 1"
                                            (infixToPostfix "(1.3|4)*.3.(4.5)+")
                                            "13.4|*3.45.+.")

infixToPostfixTest6 = TestCase (assertEqual "Complex case 2"
                                            (infixToPostfix "(((1|2)|(3|4)+)+|1)*.8")
                                            "12|34|+|+1|*8.")

infixToPostfixTest7 = TestCase (assertEqual "Complex case 3"
                                            (infixToPostfix "1.2+.3*|3.4+.2*|9")
                                            "12+.3*.34+.2*.|9|")

tests = TestList [ TestLabel "popAndPushTest1" popAndPushTest1
                 , TestLabel "popAndPushTest2" popAndPushTest2
                 , TestLabel "popAndPushTest3" popAndPushTest3
                 , TestLabel "popAndPushTest4" popAndPushTest4
                 , TestLabel "popAndPushTest5" popAndPushTest5
                 , TestLabel "popAndPushTest6" popAndPushTest6
                 , TestLabel "infixToPostfixTest1" infixToPostfixTest1
                 , TestLabel "infixToPostfixTest2" infixToPostfixTest2
                 , TestLabel "infixToPostfixTest3" infixToPostfixTest3
                 , TestLabel "infixToPostfixTest4" infixToPostfixTest4
                 , TestLabel "infixToPostfixTest5" infixToPostfixTest5
                 , TestLabel "infixToPostfixTest6" infixToPostfixTest6
                 , TestLabel "infixToPostfixTest7" infixToPostfixTest7
                 ]