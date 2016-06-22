module FirstSet where

import Grammar (Terminal)
import qualified Data.Set as Set

data FirstSet = FirstSet {
        terminals :: Set.Set Terminal,
        hasEpsilon :: Bool
    } deriving (Eq, Ord, Show)

empty :: FirstSet
empty = FirstSet Set.empty False

singleton :: Terminal -> FirstSet
singleton t = FirstSet (Set.singleton t) False

epsilon :: FirstSet
epsilon = FirstSet Set.empty True

withoutEpsilon :: FirstSet -> FirstSet
withoutEpsilon (FirstSet tt _) = FirstSet tt False

union :: FirstSet -> FirstSet -> FirstSet
union (FirstSet terminals1 eps1) (FirstSet terminals2 eps2) =
    FirstSet (terminals1 `Set.union` terminals2) (eps1 || eps2)

unions :: [FirstSet] -> FirstSet
unions = foldr union (FirstSet Set.empty False)

