import ParseLib
import Automata
import Data.String.Utils
import System.Environment
import qualified Data.Set as Set
import System.Console.Readline
import System.Exit

data LangTerm = Minimize LangTerm | Constant String | Complement LangTerm | Conjunction LangTerm LangTerm | Disjunction LangTerm LangTerm deriving (Eq, Show)

-- LangTerm parser
ab_letter	:: Parser Char
word		:: Parser String
expr		:: Parser LangTerm

ab_letter = sat (flip elem "ab")
word = do { h <- ab_letter; t <- many ab_letter; return (h:t) }
expr = term `chainl1` conj_disj
term =	do { w<-word; return (Constant w) } +++
	do { symb "-"; n<-expr; return (Complement n) } +++
	do { symb "<"; n <- expr; symb ">"; return (Minimize n) } +++
	do { symb "("; n <- expr; symb ")"; return n }
conj_disj = do { symb "&"; return Conjunction } +++ do { symb "|"; return Disjunction }

parse_term :: String -> Maybe LangTerm
parse_term s =
	if (parse expr $ trim s) == []
	then Nothing
	else
		if (snd.head) (parse expr $ trim s) /= ""
		then Nothing
		else Just ((fst.head) (parse expr $ trim s))
			where trim = replace " " ""

-- Evaluation
eval_expr :: LangTerm -> FSM Int

eval_expr (Constant s) = int_states $ ssw s (Set.fromList "ab")
eval_expr (Complement a) = co (eval_expr a)
eval_expr (Minimize a) = int_states $ minimize (eval_expr a)
eval_expr (Conjunction a b) = int_states $ (eval_expr a) &&& (eval_expr b)
eval_expr (Disjunction a b) = int_states $ (eval_expr a) ||| (eval_expr b)

-- Output
flush :: Maybe LangTerm -> IO ()
flush Nothing = putStrLn "Parse error." >> exitWith (ExitFailure 1)
flush (Just t) = (putStrLn.show) (eval_expr t) >> exitWith ExitSuccess

cli :: IO ()
cli = do {
	input <- readline "> ";
	case input of {
		Nothing -> putStrLn "bye";
		Just "" -> putStrLn "bye";
		Just s -> do {
			addHistory s;
			case parse_term s of {
				Nothing -> do { putStrLn "Parse error"; cli };
				Just x -> do { viz $ eval_expr x; cli }
			}
		}
	}
}

main = do {
	args <- getArgs;

	if concat args == ""
	then cli >> exitWith ExitSuccess
	else flush (parse_term $ concat args);
}
