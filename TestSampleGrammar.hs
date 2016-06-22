module TestSampleGrammar where

import Grammar

g0 :: Grammar
g0 = fromList [
        ("E", ["T", "E'"]),
        ("E'", ["+", "T", "E'"]),
        ("E'", []),
        ("T", ["F", "T'"]),
        ("T'", ["*", "F", "T'"]),
        ("T'", []),
        ("F", ["(", "E", ")"]),
        ("F", ["id"])
    ]

g1 :: Grammar
g1 = fromList [
        ("E", ["E", "+", "T"]),
        ("E", ["T"]),
        ("T", ["id"]),
        ("T", ["(", "E", ")"])
    ]

g2 :: Grammar
g2 = fromList [
        ("A", ["A", "x"]),
        ("A", ["x"])
    ]

g3 :: Grammar
g3 = fromList [
        ("A", ["x", "A"]),
        ("A", ["x"])
    ]

g4 :: Grammar
g4 = fromList [
        ("A", ["a", "B"]),
        ("A", ["a", "C"]),
        ("B", ["b"]),
        ("C", ["c"])
    ]

g5 :: Grammar
g5 = fromList [
        ("A", ["a", "R"]),
        ("R", ["B"]),
        ("R", ["C"]),
        ("B", ["b"]),
        ("C", ["c"])
    ]

g6 :: Grammar
g6 = fromList [
        ("S", ["A", "a", "A", "b"]),
        ("S", ["B", "b", "B", "a"]),
        ("A", []),
        ("B", [])
    ]

g7 :: Grammar
g7 = fromList [
        ("A", ["a"]),
        ("A", ["B", "x"]),
        ("B", ["b"]),
        ("B", ["A", "y"])
    ]

g8 :: Grammar
g8 = fromList [
        ("A", ["A", "a"])
    ]

g9 :: Grammar
g9 = fromList [
        ("A", ["a"]),
        ("A", ["B"]),
        ("A", ["B", "C"]),
        ("B", ["C"]),
        ("C", ["A", "b"]),
        ("C", []),
        ("D", ["E"]),
        ("E", ["e"])
    ]

g10 :: Grammar
g10 = fromList [
        ("A", ["a"]),
        ("A", ["B"]),
        ("B", ["b"]),
        ("B", ["A"])
    ]

g11 :: Grammar
g11 = fromList [
        ("A", ["a"]),
        ("A", ["E", "E", "B", "E"]),
        ("E", ["e"]),
        ("B", ["b"]),
        ("B", ["E", "A", "E"]),
        ("E", [])
    ]
