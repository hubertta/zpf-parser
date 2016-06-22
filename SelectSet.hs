module SelectSet where

import Grammar (Terminal)

import qualified Data.Set as Set

import qualified FirstSet
import FirstSet (FirstSet)

import qualified FollowSet
import FollowSet (FollowSet)

data SelectSet = SelectSet {
        terminals :: Set.Set Terminal,
        hasEpsilon :: Bool,
        hasEndOfFile :: Bool
    } deriving (Eq, Ord, Show)

empty :: SelectSet
empty = SelectSet Set.empty False False

null :: SelectSet -> Bool
null = (==empty)

fromFirstSet :: FirstSet -> SelectSet
fromFirstSet firstSet = SelectSet (FirstSet.terminals firstSet) (FirstSet.hasEpsilon firstSet) False

fromFollowSet :: FollowSet -> SelectSet
fromFollowSet followSet = SelectSet (FollowSet.terminals followSet) False (FollowSet.hasEndOfFile followSet)

union :: SelectSet -> SelectSet -> SelectSet
union (SelectSet s1 eps1 eof1) (SelectSet s2 eps2 eof2) = SelectSet (s1 `Set.union` s2) (eps1 || eps2) (eof1 || eof2)

unions :: [SelectSet] -> SelectSet
unions = foldr union empty

intersection :: SelectSet -> SelectSet -> SelectSet
intersection (SelectSet s1 eps1 eof1) (SelectSet s2 eps2 eof2) =
    SelectSet (s1 `Set.intersection` s2) (eps1 && eps2) (eof1 && eof2)