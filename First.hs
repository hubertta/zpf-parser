module First (first, firstAll) where

import Grammar
import qualified FirstSet

import qualified Data.Map as Map

import Utils (fixpoint)

import Control.Monad.State

first :: Grammar -> [RhsProdToken] -> FirstSet.FirstSet
first _ [] = FirstSet.epsilon
first g tokens = Map.findWithDefault FirstSet.empty tokens (firstAll g)

firstAll :: Grammar -> Map.Map [RhsProdToken] FirstSet.FirstSet
firstAll g @ (Grammar prods) = fixpoint iter initialMap where
    iter = execState process
    initialMap = Map.fromList $ map (\x -> ([RhsNonTerminal x], FirstSet.empty)) (nonTerminals g)

    -- Run single iteration of the algorithm
    process :: State (Map.Map [RhsProdToken] FirstSet.FirstSet) ()
    process = do
        s <- get
        -- For each key in the map try to find new elements
        sequence_ (map processTokens (Map.keys s))

    -- Try to find new first elements for given tokens sequence
    processTokens :: [RhsProdToken] -> State (Map.Map [RhsProdToken] FirstSet.FirstSet) ()
    processTokens [] = return ()
    processTokens [token] = processToken token
    processTokens tokens @ (h:t @ (_:_)) = do
        firstOfHead <- query [h]
        insert tokens (FirstSet.withoutEpsilon firstOfHead)
        when (FirstSet.hasEpsilon firstOfHead) (query t >>= insert tokens)
        processTokens t

    -- Try to find new first elements for single-token sequence
    processToken :: RhsProdToken -> State (Map.Map [RhsProdToken] FirstSet.FirstSet) ()
    processToken token @ (RhsTerminal t) = insert [token] (FirstSet.singleton t)
    processToken token @ (RhsNonTerminal nt) =
        let matchingProds = filter (\(Prod nt' _) -> nt' == nt) prods in
        let ntRhsProds = map (\(Prod _ rhs) -> rhs) matchingProds in
        do
            newMembers <- sequence $ map mapper ntRhsProds
            insert [token] (FirstSet.unions newMembers)
        where
            mapper [] = return FirstSet.epsilon
            mapper rhsTokens = query rhsTokens

    insert :: [RhsProdToken] -> FirstSet.FirstSet -> State (Map.Map [RhsProdToken] FirstSet.FirstSet) ()
    insert tokens newMembers = modify $ Map.insertWith FirstSet.union tokens newMembers

    query :: [RhsProdToken] -> State (Map.Map [RhsProdToken] FirstSet.FirstSet) FirstSet.FirstSet
    -- inserting empty set will ensure that this key will be computed in the next iteration
    query tokens = insert tokens FirstSet.empty >> get >>= return . Map.findWithDefault FirstSet.empty tokens
