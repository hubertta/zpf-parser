module Grammar where

import Data.Char (isUpper)
import qualified Data.Set as Set

type Literal = String

data Terminal = Terminal Literal deriving (Eq, Ord)
data NonTerminal = NonTerminal Literal deriving (Eq, Ord)
data RhsProdToken = RhsTerminal Terminal | RhsNonTerminal NonTerminal deriving (Eq, Ord)
data Prod = Prod NonTerminal [RhsProdToken] deriving (Eq, Ord)

data Grammar = Grammar [Prod] deriving (Eq, Ord)

instance Show Prod where
    show (Prod (NonTerminal nt) tokens) = nt ++ " -> " ++ concat (map mapper tokens) where
        mapper :: RhsProdToken -> String
        mapper (RhsTerminal (Terminal t)) = t
        mapper (RhsNonTerminal (NonTerminal nt')) = nt'

instance Show Grammar where
    show (Grammar []) = "<EMPTY GRAMMAR>"
    show (Grammar prods) = (foldr (\p r -> p ++ "\n" ++ r) "" (map show (init prods))) ++ show (last prods)

instance Show Terminal where
    show (Terminal t) = show t --"T(" ++ show t ++ ")"

instance Show NonTerminal where
    show (NonTerminal nt) = show nt --"NT(" ++ show nt ++ ")"

instance Show RhsProdToken where
    show (RhsTerminal t) = show t
    show (RhsNonTerminal nt) = show nt

startSymbol :: Grammar -> NonTerminal
startSymbol (Grammar (Prod nt _ : _)) = nt

nonTerminals :: Grammar -> [NonTerminal]
nonTerminals (Grammar prods) = get prods Set.empty where
    get [] a = Set.toList a
    get ((Prod nt _):t) a = get t (Set.insert nt a)

fromList :: [(Literal, [Literal])] -> Grammar
fromList list = Grammar prods where
    prods = map mapper list
    mapper (sNt, sRhs) = Prod (NonTerminal sNt) rhs where
        rhs = map toRhsToken sRhs
        toRhsToken lit @ (h:_) | isUpper h = RhsNonTerminal (NonTerminal lit)
                               | otherwise = RhsTerminal (Terminal lit)
