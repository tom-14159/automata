import ParseLib
import Automata
import Data.String.Utils
import System.Environment
import qualified Data.Set as Set
import System.Console.Readline

data LangTerm = Minimize LangTerm | Constant String | Complement LangTerm | Conjunction LangTerm LangTerm | Disjunction LangTerm LangTerm deriving (Eq, Show)

ab_letter :: Parser Char
ab_letter = sat (flip elem "ab")

word :: Parser String
word = do { h <- ab_letter; t <- many ab_letter; return (h:t) }

expr :: Parser LangTerm
expr = term `chainl1` conj_disj
term =	do { w<-word; return (Constant w) } +++
	do { symb "-"; n<-expr; return (Complement n) } +++
	do { symb "<"; n <- expr; symb ">"; return (Minimize n) } +++
	do { symb "("; n <- expr; symb ")"; return n }
conj_disj = do { symb "&"; return Conjunction } +++ do { symb "|"; return Disjunction }

eval_expr :: LangTerm -> FSM Int
eval_expr (Constant s) = int_states $ ssw s (Set.fromList "ab")
eval_expr (Complement a) = co (eval_expr a)
eval_expr (Minimize a) = int_states $ minimize (eval_expr a)
eval_expr (Conjunction a b) = int_states $ (eval_expr a) &&& (eval_expr b)
eval_expr (Disjunction a b) = int_states $ (eval_expr a) ||| (eval_expr b)

main = do {
	args <- getArgs;
	if (concat args) == ""
	then do {
		x <- readline "> ";

		addHistory $ is_just x;

		if snd (par $ is_just x) /= ""
		then print "Parse error\n"
		else viz ((eval_expr.fst) (par $ is_just x));

		main;
		}
	else
		if snd (par $ concat args) /= ""
		then print "Parse error\n"
		else viz ((eval_expr.fst) (par $ concat args));
	} where trim = replace " " "";
		par s = head $ parse expr (trim s);
		is_just x = case x of { Just y -> y }

