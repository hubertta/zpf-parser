module TestFirst (testFirst) where

import First
import Grammar
import qualified FirstSet
import qualified Data.Set as Set
import qualified Data.Map as Map

import TestSampleGrammar

import Test.HUnit

testFirst :: Test
testFirst = TestList [
            first_test_empty,
            first_test_E,
            first_test_E',
            first_test_T,
            first_test_T',
            first_test_F,
            first_test_S,
            first_test_A
        ]

first_test_empty :: Test
first_test_empty = TestCase $ assertEqual "" Map.empty (firstAll (Grammar []))

firstFromList :: [String] -> Bool -> FirstSet.FirstSet
firstFromList terminals epsilon =
    let temp = Set.fromList $ map Terminal terminals in
    FirstSet.FirstSet temp epsilon


make_first_test :: Grammar -> Literal -> [Literal] -> Bool -> Test
make_first_test g nt expectedTokens expectedEpsilon =
    let expected = firstFromList expectedTokens expectedEpsilon in
    let actual = first g [RhsNonTerminal (NonTerminal nt)] in
    TestCase $ assertEqual "" expected actual

first_test_E :: Test
first_test_E = make_first_test g0 "E" ["(", "id"] False

first_test_E' :: Test
first_test_E' = make_first_test g0 "E'" ["+"] True

first_test_T :: Test
first_test_T = make_first_test g0 "T" ["(", "id"] False

first_test_T' :: Test
first_test_T' = make_first_test g0 "T'" ["*"] True

first_test_F :: Test
first_test_F = make_first_test g0 "F" ["(", "id"] False

first_test_S :: Test
first_test_S = make_first_test g6 "S" ["a", "b"] False

first_test_A :: Test
first_test_A = make_first_test g6 "A" [] True
