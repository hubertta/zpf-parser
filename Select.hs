module Select (select, select2, SelectMember(..)) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified SelectSet
import qualified FirstSet
import qualified FollowSet

import Grammar
import First
import Follow

data SelectMember = SelectTerminal Terminal | SelectEof | SelectEps deriving (Eq, Ord, Show)

select :: Grammar -> Map.Map Prod SelectSet.SelectSet
select g @ (Grammar prods) = processProductions prods Map.empty where
    followMap = follow g
    processProductions [] a = a
    processProductions (h:t) a =
        let processed = processProduction h in
        processProductions t (Map.insert h processed a)

    processProduction (Prod nt tokens) =
        let firstOfTokens = first g tokens in
        let membersFromFirst = SelectSet.fromFirstSet firstOfTokens in
        case FirstSet.hasEpsilon firstOfTokens of
            False -> membersFromFirst
            True ->
                let membersFromFollow = SelectSet.fromFollowSet (Map.findWithDefault FollowSet.empty nt followMap)
                in membersFromFirst `SelectSet.union` membersFromFollow

select2 :: Grammar -> Map.Map (SelectMember, NonTerminal) [[RhsProdToken]]
select2 g =
    let sel = select g in
    Map.foldrWithKey f Map.empty sel where
        f (Prod nt rhs) (SelectSet.SelectSet terminals eps eof) select2set =
           let withEps = case eps of
                    False -> select2set
                    True -> Map.insertWith (++) (SelectEps, nt) [rhs] select2set in
           let withEof = case eof of
                    False -> withEps
                    True -> Map.insertWith (++) (SelectEof, nt) [rhs] withEps in
           addForTerminals (Set.toList terminals) withEof where
               addForTerminals [] a = a
               addForTerminals (t:tt) a = addForTerminals tt $ Map.insertWith (++) (SelectTerminal t, nt) [rhs] a

