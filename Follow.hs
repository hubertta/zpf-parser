module Follow where

import Grammar
import qualified FirstSet
import qualified FollowSet

import First

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

import Utils (fixpoint)

follow :: Grammar -> Map.Map NonTerminal FollowSet.FollowSet
follow g @ (Grammar prods) = fixpoint iter initialMap where
        initialMap = Map.singleton (startSymbol g) (FollowSet.FollowSet Set.empty True)
        iter = execState (processProductions prods)

        processProductions :: [Prod] -> State (Map.Map NonTerminal FollowSet.FollowSet) ()
        processProductions [] = return ()
        processProductions ((Prod nt rhs):t) = do
            processProduction nt rhs
            processProductions t

        processProduction :: NonTerminal -> [RhsProdToken] -> State (Map.Map NonTerminal FollowSet.FollowSet) ()
        processProduction nt (RhsNonTerminal nt2:t @ (_:_)) = do
            let firstOfTail = first g t
            let newMembersFromTail = FollowSet.FollowSet (FirstSet.terminals firstOfTail) False

            newMembersFromNt <- case FirstSet.hasEpsilon firstOfTail of
                    False -> return FollowSet.empty
                    True -> do
                        s <- get
                        return $ Map.findWithDefault FollowSet.empty nt s

            let newMembers = newMembersFromTail `FollowSet.union` newMembersFromNt
            modify $ Map.insertWith FollowSet.union nt2 newMembers
            processProduction nt t
        processProduction nt [(RhsNonTerminal nt2)] = do
            s <- get
            let newMembers = Map.findWithDefault FollowSet.empty nt s
            modify $ Map.insertWith FollowSet.union nt2 newMembers
        processProduction nt ((RhsTerminal _):t) = processProduction nt t
        processProduction _ [] = return ()

