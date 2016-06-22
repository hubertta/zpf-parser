module LLLib where

import qualified Grammar
import Grammar (Grammar(..), RhsProdToken(..), Prod(..), Terminal(..), NonTerminal(..), Literal)
import Select
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified SelectSet

import Utils

data Tree = Leaf RhsProdToken | Branch NonTerminal [Tree] deriving (Eq, Ord, Show)

data ParserToken = TerminalToken Literal | EndOfFileToken deriving (Eq, Ord, Show)


analizaLL :: Grammar -> [Terminal] -> Either Tree String
analizaLL g terminals =
    let initialTokens = (map (\(Terminal l) -> TerminalToken l) terminals) ++ [EndOfFileToken] in
    let initialTree = Leaf (RhsNonTerminal $ Grammar.startSymbol g) in
    let initialStack = [RhsNonTerminal $ Grammar.startSymbol g] in
    analizuj initialTokens initialStack initialTree where
        analizuj [EndOfFileToken] [] a = Left a

        analizuj [] _ _ = Right "Word does not belong to this grammar (1)"
        analizuj _ [] _ = Right "Word does not belong to this grammar (2)"

        analizuj (TerminalToken t:tt) (RhsTerminal (Terminal t'):tt') a = case t == t' of
            True -> analizuj tt tt' a
            False -> Right "Word does not belong to this grammar (3)"

        analizuj _ (RhsTerminal _:_) _ = Right "Word does not belong to this grammar (4)"

        analizuj (TerminalToken t:tt) (RhsNonTerminal (NonTerminal nt):nt') a =
            let maybeRhs = case Map.lookup (SelectTerminal (Terminal t), NonTerminal nt) selectSet of
                    Just [rhs] -> Left rhs
                    Nothing -> case Map.lookup (SelectEps, NonTerminal nt) selectSet of
                        Just [rhs] -> Left rhs
                        Nothing -> Right "Word does not belong to this grammar (5)"
                        Just _ -> Right "Grammar is not LL1 (1)"
                    Just _ -> Right "Grammar is not LL1 (2)"
            in case maybeRhs of
                Left rhs -> analizuj (TerminalToken t:tt) (rhs ++ nt') (aktualizujDrzewo a (NonTerminal nt) rhs)
                Right msg -> Right msg

        analizuj (EndOfFileToken:tt) (RhsNonTerminal (NonTerminal nt):nt') a =
            case Map.lookup (SelectEof, NonTerminal nt) selectSet of
                Just [rhs] -> analizuj (EndOfFileToken:tt) (rhs ++ nt') (aktualizujDrzewo a (NonTerminal nt) rhs)
                Nothing -> Right "Word does not belong to this grammar (6)"
                Just _ -> Right "Grammar is not LL1 (3)"

        selectSet = select2 g

        aktualizujDrzewo :: Tree -> NonTerminal -> [RhsProdToken] -> Tree
        aktualizujDrzewo (Branch nt' children) nt rhs = Branch nt' $ aktualizujDzieci children [] rhs nt
        aktualizujDrzewo (Leaf (RhsNonTerminal nt')) nt rhs | nt' == nt = Branch nt' (map Leaf rhs)
        aktualizujDrzewo _ _ _ = error "bug in aktualizujDrzewo"

        aktualizujDzieci [] a _ _ = a
        aktualizujDzieci (t:tt) a rhs nt | containsNt nt t = reverse a ++ [(aktualizujDrzewo t nt rhs)] ++ tt
        aktualizujDzieci (t:tt) a rhs nt | otherwise = aktualizujDzieci tt (t:a) rhs nt

        containsNt nt (Leaf (RhsNonTerminal nt')) = nt == nt'
        containsNt _ (Leaf (RhsTerminal _)) = False
        containsNt nt (Branch _ children) = any (containsNt nt) children


jestLL1 :: Grammar -> Bool
jestLL1 g =
    let selectSets = select g in
    let union prod @ (Prod nt _) = -- union of all select sets mathing nt of given production, except for the one corresponding to given production
            let relevantSets = Map.filterWithKey (\(Prod k _) _ -> k == nt) selectSets in
            let mapWithoutProd = Map.delete prod relevantSets in
            let setList = (map snd . Map.toList) mapWithoutProd in
            SelectSet.unions setList
    in
    forallWithKey (\prod set -> SelectSet.null $ set `SelectSet.intersection` union prod) selectSets where
        forallWithKey :: (k -> v -> Bool) -> Map.Map k v -> Bool
        forallWithKey keyValuePred = Map.foldrWithKey f True where
            f k v a = a && (keyValuePred k v)


jestCykl :: Grammar -> Bool
jestCykl g @ (Grammar prods) = hasCycle startSymbol initialPath stepsLeft where
    startSymbol = Grammar.startSymbol g
    initialPath = []
    stepsLeft = length (Grammar.nonTerminals g) + 2

    hasCycle :: NonTerminal -> [NonTerminal] -> Int -> Bool
    hasCycle _ _ 0 = False
    hasCycle s p n = case p of
        (h:_) | h == s -> True
        _ ->
            let next = case p of
                    [] -> nextNonTerminals s
                    h:_ -> nextNonTerminals h
            in any (\nextNonTerminal -> hasCycle s (nextNonTerminal:p) (n - 1)) next

    nextNonTerminals nt = nextNonTerminalsHelper Set.empty relevantRhss where
        relevantProds = filter (\(Prod nt' _) -> nt' == nt) prods
        relevantRhss = map (\(Prod _ rhs) -> rhs) relevantProds

        nextNonTerminalsHelper a [] = a
        nextNonTerminalsHelper a (rhs:t) =
            let fromThisRhs = nextNonTerminalsFromRhs rhs
            in nextNonTerminalsHelper (fromThisRhs `Set.union` a) t

        nextNonTerminalsFromRhs rhs = choose [] rhs Set.empty where
            choose _ [] a = a
            choose lhs (s @ (RhsNonTerminal x):rhs') a
                | nullable g lhs && nullable g rhs' = choose (s:lhs) rhs' (x `Set.insert` a)
                | otherwise = choose (s:lhs) rhs' a
            choose _ (RhsTerminal _:_) _ = Set.empty


nullable :: Grammar -> [RhsProdToken] -> Bool
nullable g = all (\e -> e `elem` nullableNonTerminals g)


nullableNonTerminals :: Grammar -> [RhsProdToken]
nullableNonTerminals (Grammar prods) =
    let productionsWithRemovedNullableNonTerminals = fixpoint removeDirectlyNullableNonTerminals prods in
    let nonTerminals = directlyNullableNonTerminals productionsWithRemovedNullableNonTerminals in
    map (\x -> RhsNonTerminal x) nonTerminals where
        removeDirectlyNullableNonTerminals prods' = map (removeFromRhs directlyNullable) prods' where
            directlyNullable = directlyNullableNonTerminals prods'

        directlyNullableNonTerminals prods' = filter isNull prods' >>= \(Prod nt _) -> return nt

        removeFromRhs nonTerminals (Prod nt rhs) =
            let newRhs = filter shouldBeRetained rhs in
            Prod nt newRhs where
                shouldBeRetained (RhsTerminal _) = True
                shouldBeRetained (RhsNonTerminal nt') = notElem nt' nonTerminals

        isNull (Prod _ []) = True
        isNull _ = False

