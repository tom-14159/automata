module Automata where

import Data.List (find,findIndex)
import Data.Set (Set, fromList, singleton, member, toList, size, union, difference, intersection, empty, fold, insert, findMin)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.String.Utils
import System.Exit
import System.Cmd

type Alpha = Set Char
type State t = t
type Delta t = State t -> Char -> State t

data FSM t = DFA (Set (State t), Alpha, Delta t, State t, Set (State t))

escape :: String -> String
escape s = "\"" ++ replace "\"" "\\\"" s ++ "\""

gvtrans :: Show t => FSM t -> State t -> [String]
gvtrans (DFA (s, al, d, i, f)) st = map (\a->"\t" ++ (escape.show) st ++ " -> " ++ (escape.show) (d st a) ++ " [ label = \""++[a]++"\" ];" ) (toList al)

instance (Show t) => Show (FSM t) where
	show dfa@(DFA (s, a, d, i, f)) = unlines $ [
			"digraph finite_state_machine {",
			"\trankdir=LR;",
			"\tsize=\"8,5\";",
			"\tnode [shape = doublecircle];",
			"\t"++(join " " $ map (escape.show) $ toList f)++(if size f > 0 then ";" else ""),
			"\tnode [shape = circle];"
		] ++ concat (map (gvtrans dfa) (toList s)) ++ [
			"}"
		]

co :: Ord (State t) => FSM t -> FSM t
co (DFA (s, a, d, i, f)) = DFA (s, a, d, i, s `difference` f)

empty_lang :: Alpha -> FSM Int
empty_lang a = DFA (singleton 0, a, const, 0, empty)

reachable_states :: Ord (State t) =>
	FSM t	->	-- automaton
	Set t	->	-- reached states
	[t]	->	-- states to explore
	Set t		-- reachable states
reachable_states _ r [] = r
reachable_states m@(DFA (s, a, d, i, f)) reached (h:t) =
	reachable_states m (reached `union` new_found) (toList(new_found `difference` reached) ++ t)
		where new_found = S.map (d h) a

reachable :: Ord (State t) => FSM t -> FSM  t
reachable m@(DFA (s, a, d, i, f)) = DFA (r, a, d, i, f `intersection` r) where r = reachable_states m (singleton i) [i]

product_set :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
product_set a b = fromList $ [(x,y) | x<-(toList a), y<-(toList b)]

leads_to :: Ord t => FSM t -> State t -> [State t]
leads_to m@(DFA (s, a, d, i, f)) st = map (d st) (toList a)

new_classes :: Ord t => FSM t -> [Set (State t)] -> [(State t, [Int])]
new_classes m@(DFA (s, a, d, i, f)) partition = toList $ S.map (\st->(st, (get_index partition st : map (get_index partition) (leads_to m st)))) s
	where get_index p e = case findIndex (member e) p of { Just x -> x }

refine :: Ord t => FSM t -> [Set (State t)] -> [Set (State t)]
refine m p = map (\k -> fromList (map fst (filter (\x->snd x == k) ns))) (toList (fromList (map snd ns))) where ns = new_classes m p

refine_many :: Ord t => FSM t -> [Set (State t)] -> [Set (State t)]
refine_many m p = if length p /= length r then refine_many m r else r where r = refine m p

minimize :: Ord t => FSM t -> FSM (Set (State t))
minimize m@(DFA (s, a, d, i, f)) = DFA (new_states, a, (\st al->get_class new_states (d (findMin st) al)), get_class new_states i, S.map (get_class new_states) f)
	where get_class p i = fold (\x y -> if i `member` x then x else y) empty p;
		new_states = fromList $ refine_many m [s `difference` f, f]

product :: (Ord (State s), Ord (State t)) => FSM s -> FSM t -> (Set s -> Set t -> Set (s,t)) -> FSM (s,t)
product (DFA (s1, a1, d1, i1, f1)) (DFA (s2, a2, d2, i2, f2)) f =
	if a1 == a2
	then DFA (
		product_set s1 s2,
		a1,
		\(x,y) a -> (d1 x a, d2 y a),
		(i1,i2),
		f f1 f2
		)
	else error "Product of Automata over different alphabets!"

(&&&) :: (Ord (State s), Ord (State t)) => FSM s -> FSM t -> FSM (s,t)
(&&&) (DFA a) (DFA b) =
	reachable (Automata.product (DFA a) (DFA b) product_set)

(|||) :: (Ord (State s), Ord (State t)) => FSM s -> FSM t -> FSM (s,t)
(|||) m1@(DFA (s1, a1, d1, i1, f1)) m2@(DFA (s2, a2, d2, i2, f2)) =
	reachable (Automata.product m1 m2 (\st1 st2->product_set s1 st2 `union` product_set st1 s2))

elem_index :: Eq a => a -> [a] -> Int
elem_index x (h:t) = if x==h then 1 else 1 + elem_index x t

int_states :: (Ord (State s)) => FSM s -> FSM Int
int_states (DFA (s, a, d, i, f)) =
	DFA (fromList [1..size s], a, (\st al->get_index $ d (list_state!!(st-1)) al), get_index i, S.map get_index f)
		where list_state = toList s; get_index = flip elem_index list_state

ssw_delta :: Set String -> String -> Char -> String
ssw_delta st s a = if (s++[a]) `member` st then s++[a] else s

ssw :: String -> Alpha -> FSM String
ssw w al = DFA (states, al, ssw_delta states, "", singleton w) where states = fromList $ map (flip take w) [0..length w]

subword :: String -> FSM String
subword s = ssw s (fromList "ab")

