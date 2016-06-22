module TestLLLib (testLLLib) where

import Test.HUnit

import LLLib
import Grammar

import TestSampleGrammar

testLLLib :: Test
testLLLib = TestList [
        testAnalysis,
        testCycles
    ]

testAnalysis :: Test
testAnalysis = TestList [
        makeAnalysisTest g6 ["a"] Nothing,
        makeAnalysisTest g6 ["a", "b"] $ Just $ Branch (NonTerminal "S") [
                Branch (NonTerminal "A") [],
                Leaf (RhsTerminal (Terminal "a")),
                Branch (NonTerminal "A") [],
                Leaf (RhsTerminal (Terminal "b"))
            ],
        makeAnalysisTest g6 ["b", "a"] $ Just $ Branch (NonTerminal "S") [
                Branch (NonTerminal "B") [],
                Leaf (RhsTerminal (Terminal "b")),
                Branch (NonTerminal "B") [],
                Leaf (RhsTerminal (Terminal "a"))
            ],
        makeAnalysisTest g5 ["a", "c"] $ Just $ Branch (NonTerminal "A") [
                Leaf (RhsTerminal (Terminal "a")),
                Branch (NonTerminal "R") [
                        Branch (NonTerminal "C") [
                                Leaf (RhsTerminal (Terminal "c"))
                            ]
                    ]
            ]
    ]

testCycles :: Test
testCycles = TestList $ map (\(g, e) -> makeCycleTest g e) [
        (g0, False),
        (g1, False),
        (g2, False),
        (g3, False),
        (g4, False),
        (g5, False),
        (g6, False),
        (g7, False),
        (g8, False),
        (g9, False),
        (g10, True),
        (g11, True)
    ]

makeCycleTest :: Grammar -> Bool -> Test
makeCycleTest g expected =
    let actual = jestCykl g in
    TestCase $ assertEqual "cycle" expected actual

makeAnalysisTest :: Grammar -> [Literal] -> Maybe Tree -> Test
makeAnalysisTest g tokens expected =
    let actual = analizaLL g $ map Terminal tokens in
    TestCase $ helper actual expected where
        helper (Left actual') (Just expected') = assertEqual "analysis" expected' actual'
        helper (Right _) Nothing = assertBool "analysis" True
        helper _ _ = assertBool "analysis" False
