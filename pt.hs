import ParseLib
import Automata
import Data.String.Utils
import qualified Data.Set as Set

data LangTerm = Constant String | Complement LangTerm | Conjunction LangTerm LangTerm | Disjunction LangTerm LangTerm deriving (Eq, Show)

ab_letter :: Parser Char
ab_letter = sat (flip elem "ab")

word :: Parser String
word = do { h <- ab_letter; t <- many ab_letter; return (h:t) }

expr :: Parser LangTerm
expr = term `chainl1` conj_disj
term = do { w<-word; return (Constant w) } +++ do { symb "-"; n<-expr; return (Complement n) } +++ do { symb "("; n <- expr; symb ")"; return n }
conj_disj = do { symb "&"; return Conjunction } +++ do { symb "|"; return Disjunction }

eval_expr :: LangTerm -> FSM Int
eval_expr (Constant s) = int_states $ ssw s (Set.fromList "ab")
eval_expr (Complement a) = co (eval_expr a)
eval_expr (Conjunction a b) = int_states $ (eval_expr a) &&& (eval_expr b)
eval_expr (Disjunction a b) = int_states $ (eval_expr a) ||| (eval_expr b)

main = do {
	x <- getLine;
	if snd (par x) /= "" then print "Parse error\n" else viz ((eval_expr.fst) (par x));
	main;
	} where trim = replace " " "";
		par s = head $ parse expr (trim s)

