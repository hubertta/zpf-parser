module TestSelect (testSelect) where

import Test.HUnit

import LLLib
import Grammar
import TestSampleGrammar

testSelect :: Test
testSelect = TestList [
        testLL1 g0 True,
        testLL1 g1 False,
        testLL1 g2 False,
        testLL1 g3 False,
        testLL1 g4 False,
        testLL1 g5 True,
        testLL1 g6 True,
        testLL1 g7 False
    ]

testLL1 :: Grammar -> Bool -> Test
testLL1 g expected =
    let actual = jestLL1 g in
    TestCase $ assertEqual "jestLL1" expected actual
