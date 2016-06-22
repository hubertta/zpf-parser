module TestFollow (testFollow) where

import Test.HUnit

import Follow
import qualified FollowSet
import Grammar

import qualified Data.Set as Set
import qualified Data.Map as Map

import TestSampleGrammar

testFollow :: Test
testFollow = TestList [follow_test]

make_follow_test :: [(Literal, ([Literal], Bool))] -> Test
make_follow_test pairs =
    let expected_map = Map.fromList $ map mapper pairs in
    let actual_map = follow g0 in
    TestCase $ assertEqual "" expected_map actual_map where
        mapper :: (Literal, ([Literal], Bool)) -> (NonTerminal, FollowSet.FollowSet)
        mapper (sNt, (sTerminals, hasEof)) = (NonTerminal sNt, terminalsSet) where
            terminalsSet = FollowSet.FollowSet (Set.fromList $ map Terminal sTerminals) hasEof

follow_test :: Test
follow_test = make_follow_test [
        ("E", ([")"], True)),
        ("E'", ([")"], True)),
        ("T", (["+", ")"], True)),
        ("T'", (["+", ")"], True)),
        ("F", (["*", "+", ")"], True))
    ]
