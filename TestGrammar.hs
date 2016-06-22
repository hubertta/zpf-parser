module TestGrammar (testGrammar) where

import Grammar
import TestSampleGrammar

import Test.HUnit

testGrammar :: Test
testGrammar = TestList [testFromList]

testFromList =
    let g0explicit = Grammar [
                Prod (NonTerminal "E") [RhsNonTerminal (NonTerminal "T"), RhsNonTerminal (NonTerminal "E'")],
                Prod (NonTerminal "E'") [RhsTerminal (Terminal "+"), RhsNonTerminal (NonTerminal "T"), RhsNonTerminal (NonTerminal "E'")],
                Prod (NonTerminal "E'") [],
                Prod (NonTerminal "T") [RhsNonTerminal (NonTerminal "F"), RhsNonTerminal (NonTerminal "T'")],
                Prod (NonTerminal "T'") [RhsTerminal (Terminal "*"), RhsNonTerminal (NonTerminal "F"), RhsNonTerminal (NonTerminal "T'")],
                Prod (NonTerminal "T'") [],
                Prod (NonTerminal "F") [RhsTerminal (Terminal "("), RhsNonTerminal (NonTerminal "E"), RhsTerminal (Terminal ")")],
                Prod (NonTerminal "F") [RhsTerminal (Terminal "id")]
            ] in
    TestCase $ assertEqual "Grammar.fromList" g0explicit g0
