module NfaTests where

import Test.HUnit
import Nfa
import Data.List

literalA = literalConstruction (Symbol 'A')
literalB = literalConstruction (Symbol 'B')
literalC = literalConstruction (Symbol 'C')

complex1 = concatenateNfas literalA literalB
complex2 = unionNfas literalB literalC
complex3 = concatenateNfas complex1 complex2

test1 = TestCase (assertBool "Test 1"
                             $ literalA `accepts` [Symbol 'A'])

test2 = TestCase (assertBool "Test 2"
                             $ not (literalA `accepts` [Symbol 'B']))

test3 = TestCase (assertBool "Test 3"
                             $ complex1 `accepts` [Symbol 'A', Symbol 'B'])

test4 = TestCase (assertBool "Test 4"
                             $ not (complex1 `accepts` [Symbol 'B', Symbol 'A']))

test5 = TestCase (assertBool "Test 5"
                             $ complex2 `accepts` [Symbol 'B'])

test6 = TestCase (assertBool "Test 6"
                             $ not $ complex2 `accepts` [Symbol 'A'])

test7 = TestCase (assertBool "Test 7"
                             $ complex3 `accepts` [Symbol 'A', Symbol 'B', Symbol 'B'])

test8 = TestCase (assertBool "Test 8"
                             $ complex3 `accepts` [Symbol 'A', Symbol 'B', Symbol 'C'])

tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2
                 , TestLabel "test3" test3
                 , TestLabel "test4" test4
                 , TestLabel "test5" test5
                 , TestLabel "test6" test6
                 , TestLabel "test7" test7
                 , TestLabel "test8" test8
                 ]