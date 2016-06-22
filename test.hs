module Test where

import Test.HUnit

import TestFirst
import TestFollow
import TestSelect
import TestNullableNonTerminals
import TestLLLib

main :: IO ()
main = do
    -- putStrLn "Running tests for Grammar"
    -- _ <- runTestTT testGrammar

    putStrLn "Running tests for First"
    _ <- runTestTT testFirst

    putStrLn "Running tests for Follow"
    _ <- runTestTT testFollow

    putStrLn "Running tests for NullableNonTerminals"
    _ <- runTestTT testNullableNonTerminals

    putStrLn "Running tests for Select / jestLL1"
    _ <- runTestTT testSelect

    putStrLn "Running tests for LLLib"
    _ <- runTestTT testLLLib

    return ()
