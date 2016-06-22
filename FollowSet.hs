module FollowSet where

import Grammar (Terminal)
import qualified Data.Set as Set

data FollowSet = FollowSet {
        terminals :: Set.Set Terminal,
        hasEndOfFile :: Bool
    } deriving (Eq, Ord, Show)

empty :: FollowSet
empty = FollowSet Set.empty False

union :: FollowSet -> FollowSet -> FollowSet
union (FollowSet terminals1 eof1) (FollowSet terminals2 eof2) =
    FollowSet (terminals1 `Set.union` terminals2) (eof1 || eof2)

unions :: [FollowSet] -> FollowSet
unions = foldr union empty
