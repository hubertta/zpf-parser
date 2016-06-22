module TestNullableNonTerminals (testNullableNonTerminals) where

import Test.HUnit

import LLLib

import TestSampleGrammar
import Grammar

import qualified Data.Set as Set

testNullableNonTerminals :: Test
testNullableNonTerminals = TestList [
        testDirectlyNullable,
        testIndirectlyNullable
    ]

makeTest :: Grammar -> [Literal] -> Test
makeTest g expectedNts =
    let actual = Set.fromList $ nullableNonTerminals g in
    let expected = Set.fromList $ map (\s -> RhsNonTerminal (NonTerminal s)) expectedNts in
    TestCase $ assertEqual "nullable non terminals" expected actual

testDirectlyNullable :: Test
testDirectlyNullable = makeTest g6 ["A", "B"]

testIndirectlyNullable :: Test
testIndirectlyNullable = makeTest g9 ["A", "B", "C"]
