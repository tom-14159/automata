import ParseLib
import Automata
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Utils
import System.Environment
import System.Console.Readline
import System.Exit
import System.IO.Temp
import System.Cmd
import System.Directory

data LangTerm = Minimize LangTerm | Constant String | Complement LangTerm | Conjunction LangTerm LangTerm | Disjunction LangTerm LangTerm deriving (Eq, Show)
type Env = (
	String,		-- alphabet
	String		-- automata / biautomata
	)

defaultEnv :: Env
defaultEnv = ("ab", "dfa")

env_alpha :: Env -> String
env_alpha = fst

env_new_alpha :: Env -> String -> Env
env_new_alpha env a = (S.toList $ S.fromList (replace " " "" a), snd env)

-- LangTerm parser
ab_letter	:: String -> Parser Char
word		:: String -> Parser String
expr		:: String -> Parser LangTerm

ab_letter a = sat (flip elem a)
word a = do { h <- ab_letter a; t <- many (ab_letter a); return (h:t) }
expr a = term a `chainl1` conj_disj a
term a =do { w<-word a; return (Constant w) } +++
	do { symb "-"; n<-term a; return (Complement n) } +++
	do { symb "<"; n <- expr a; symb ">"; return (Minimize n) } +++
	do { symb "("; n <- expr a; symb ")"; return n }
conj_disj a = do { symb "&"; return Conjunction } +++ do { symb "|"; return Disjunction }

parse_term :: String -> String -> Maybe LangTerm
parse_term alpha s =
	if (parse bound_expr $ trim s) == []
	then Nothing
	else
		if (snd.head) (parse bound_expr $ trim s) /= ""
		then Nothing
		else Just ((fst.head) (parse bound_expr $ trim s))
			where trim = replace " " "";
				bound_expr = expr alpha

-- Evaluation
eval_expr :: String -> LangTerm -> FSM Int

eval_expr alpha (Constant s) = int_states $ ssw s (S.fromList alpha)
eval_expr alpha (Complement a) = co (eval_expr alpha a)
eval_expr alpha (Minimize a) = int_states $ minimize (eval_expr alpha a)
eval_expr alpha (Conjunction a b) = int_states $ (eval_expr alpha a) &&& (eval_expr alpha b)
eval_expr alpha (Disjunction a b) = int_states $ (eval_expr alpha a) ||| (eval_expr alpha b)

-- Output
flush :: Env -> Maybe LangTerm -> IO ()
flush env Nothing = putStrLn "Parse error." >> exitWith (ExitFailure 1)
flush env (Just t) = (putStrLn.show) (eval_expr (env_alpha env) t) >> exitWith ExitSuccess

viz' :: (Ord t, Show t) => String -> FSM t -> IO ()
viz' driver m = withSystemTempDirectory "piecewise" (writeAndShow driver m) >> return ()

writeAndShow :: (Ord t, Show t) => String -> FSM t -> String -> IO ()
writeAndShow driver m path = do {
	writeFile (path++".dot") (show m);
	exitCode <- system (driver ++ " -Tpdf "++path++".dot -o "++path++".pdf");
	if exitCode == ExitSuccess
	then do {
		system ("evince "++path++".pdf");
		return ();
	} else do {
		putStrLn (driver ++ " failed");
	};

	removeFile (path++".pdf");
	removeFile (path++".dot");
}

cli :: Env -> IO ()
cli env = do {
	input <- readline "> ";
	case input of {
		Nothing -> putStrLn "bye";
		Just "" -> putStrLn "bye";
		Just s -> do {
			addHistory s;
			case s of {
				('@':alpha) -> do { cli $ env_new_alpha env alpha };
				s -> case parse_term (env_alpha env) s of {
					Nothing -> do { putStrLn "Parse error"; cli env };
					Just x -> do { viz' "dot" $ eval_expr (env_alpha env) x; cli env }
				}
			}
		}
	}
}

main = do {
	args <- getArgs;

	if concat args == ""
	then cli defaultEnv >> exitWith ExitSuccess
	else flush defaultEnv (parse_term (env_alpha defaultEnv) $ concat args);
}
