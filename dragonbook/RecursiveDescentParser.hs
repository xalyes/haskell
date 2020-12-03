module RecursiveDescentParser where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Debug.Trace
import Control.Monad.Writer
import Control.Monad.State
import System.IO

import Data.Map (Map)
import qualified Data.Map as Map

data Type = Boolean | Integ | Character
    deriving (Eq, Ord)

instance Show Type where
    show typ = case typ of
                Boolean -> "bool"
                Integ -> "int"
                Character -> "char"

data Token = Plus | Minus | Div | Mul 
            | More | Less | MoreEq | LessEq | Equal | NotEqual 
            | Num Int | Id String | Type Type | If | While
            | LeftBrace | RightBrace | Semicolon | Assign | LeftParenthese | RightParenthese
    deriving (Show, Eq, Ord)

extractDigit :: String -> Int -> (Int, String)
extractDigit [] acc = (acc, [])
extractDigit (x:xs) acc | isDigit x = extractDigit xs (acc*10 + (digitToInt x))
                        | isLetter x = error "Syntax error: number before letter"
                        | otherwise = (acc, (x:xs))

extractWord :: String -> String -> (Token, String)
extractWord [] acc = case acc of  "bool" -> (Type Boolean, [])
                                  "int"  -> (Type Integ, [])
                                  "char" -> (Type Character, [])
                                  "if"   -> (If, [])
                                  "while"-> (While, [])
                                  _      -> (Id acc, [])
extractWord (x:xs) acc | isLetter x || isDigit x = extractWord xs (acc ++ [x])
                       | otherwise = case acc of  "bool" -> (Type Boolean, (x:xs))
                                                  "int"  -> (Type Integ, (x:xs))
                                                  "char" -> (Type Character, (x:xs))
                                                  "if"   -> (If, (x:xs))
                                                  "while"-> (While, (x:xs))
                                                  _      -> (Id acc, (x:xs))

waitSymbol :: Char -> String -> String
waitSymbol ch [] = error "Syntax error: unclosed comment"
waitSymbol ch (x:xs) = if ch == x then xs else waitSymbol ch xs

skipComment :: String -> String
skipComment [] = error "Syntax error: failed to parse single '/' - allowed /* and //"
skipComment (x:xs) | x == '*'  = case waitSymbol '*' xs of []       -> error "Syntax error: unclosed comment"
                                                           (x':xs')  -> if x' == '/' then xs' else skipComment ('*':x':xs')
                   | x == '/'  = waitSymbol '\n' xs
                   | otherwise = error "Syntax error: unexpected symbol after '/'"

scanInternal :: String -> [Token] -> [Token]
scanInternal [] tokens                             = tokens
scanInternal (x:xs) tokens | x == ' ' || x == '\t' || x == '\n' = scanInternal xs tokens
                   | x == ';'                           = tokens ++ [Semicolon] ++ scanInternal xs []
                   | x == '{'                           = tokens ++ [LeftBrace] ++ scanInternal xs []
                   | x == '}'                           = tokens ++ [RightBrace] ++ scanInternal xs []
                   | x == '('                           = tokens ++ [LeftParenthese] ++ scanInternal xs []
                   | x == ')'                           = tokens ++ [RightParenthese] ++ scanInternal xs []
                   | x == '+'                           = tokens ++ [Plus] ++ scanInternal xs []
                   | x == '-'                           = tokens ++ [Minus] ++ scanInternal xs []
                   | isDigit x                          = let (digit, rest) = extractDigit xs (digitToInt x)
                                                             in tokens ++ [Num digit] ++ scanInternal rest []
                   | isLetter x                         = let (word, rest) = extractWord xs [x]
                                                             in tokens ++ [word] ++ scanInternal rest []
                   | x == '/' && xs /= [] && (head xs == '*' || head xs == '/') = scanInternal (skipComment xs) tokens
                   | x == '/'                           = tokens ++ [Div] ++ scanInternal xs []
                   | x == '*'                           = tokens ++ [Mul] ++ scanInternal xs []
                   | x == '<' && xs /= [] && (head xs == '=') = tokens ++ [LessEq] ++ scanInternal (tail xs) []
                   | x == '>' && xs /= [] && (head xs == '=') = tokens ++ [MoreEq] ++ scanInternal (tail xs) []
                   | x == '=' && xs /= [] && (head xs == '=') = tokens ++ [Equal] ++ scanInternal (tail xs) []
                   | x == '!' && xs /= [] && (head xs == '=') = tokens ++ [NotEqual] ++ scanInternal (tail xs) []
                   | x == '>'                           = tokens ++ [More] ++ scanInternal xs []
                   | x == '<'                           = tokens ++ [Less] ++ scanInternal xs []
                   | x == '='                           = tokens ++ [Assign] ++ scanInternal xs []
                   | otherwise                          = error "Syntax error: unexpected symbol"

scan str = scanInternal str []

data Factor = FactorExpr Expression | FactorNum Int | FactorStr String
    deriving (Show, Eq)

data Term = TermMul Factor Term | TermFactor Factor
    deriving (Show, Eq)

data Add = AddPlus Term Add | AddTerm Term
    deriving (Show, Eq)

data Rel = RelLess Add Rel | RelLessEq Add Rel | RelAdd Add
    deriving (Show, Eq)

data Expression = ExprRel Rel | ExprAssign Rel Expression
    deriving (Show, Eq)

data Statement = StatementExpr Expression | StmtIf Expression SyntaxTree | StmtWhile Expression SyntaxTree | DoWhile Statement Expression
    deriving (Show, Eq)

data SyntaxElement = Root | Block | Statement Statement | Declaration Type String
    deriving (Show, Eq)

data SyntaxTree = Nil | Node SyntaxElement [(SyntaxTree)]
    deriving (Eq, Show)

parseDecl :: [Token] -> Type -> (Writer String (SyntaxTree, [Token]))
parseDecl tokens typ = case tokens of
                   ((Id name):Semicolon:xs) -> return (Node (Declaration typ name) [], xs)
                   _         -> error "Syntax error: failed to parse declaration"

parseFactor :: [Token] -> (Factor, [Token])
-- parseFactor expr | traceShow (expr) False = undefined
parseFactor (x:xs) = case x of
                        (Num num)      -> (FactorNum num, xs)
                        (Id str)       -> (FactorStr str, xs)
                        LeftParenthese -> let (expr, rest) = parseExpression xs
                                          in case rest of
                                                (RightParenthese:rest') -> (FactorExpr expr, rest')
                                                _ -> error "Syntax error: failed to parse match right parenthese with left"
                        _              -> error "Syntax error: failed to parse factor"

parseTerm :: [Token] -> (Term, [Token])
parseTerm tokens = let (factor, rest) = parseFactor tokens
                   in case rest of 
                        (Mul:xs) -> let (term, rest') = parseTerm xs
                                    in (TermMul factor term, rest')
                        _        -> (TermFactor factor, rest)

parseAdd :: [Token] -> (Add, [Token])
parseAdd tokens = let (term, rest) = parseTerm tokens
                  in case rest of 
                        (Plus:xs) -> let (add, rest') = parseAdd xs
                                     in (AddPlus term add, rest')
                        _         -> (AddTerm term, rest)

parseRel :: [Token] -> (Rel, [Token])
parseRel tokens = let (add, rest) = parseAdd tokens
                  in case rest of 
                        (LessEq:xs) -> let (rel, rest') = parseRel xs
                                       in (RelLessEq add rel, rest')
                        (Less:xs)   -> let (rel, rest') = parseRel xs
                                       in (RelLess add rel, rest')
                        _           -> (RelAdd add, rest)

parseExpression :: [Token] -> (Expression, [Token])
parseExpression tokens = let (rel, rest) = parseRel tokens
                  in case rest of 
                        (Assign:xs) -> let (expr, rest') = parseExpression xs
                                       in (ExprAssign rel expr, rest')
                        _           -> (ExprRel rel, rest)

parseStatement :: [Token] -> (SyntaxTree, [Token])
parseStatement []         = error "Syntax error: failed to parse statement - unexpected end"
parseStatement (t:tokens) | t == If   = case tokens of
                                            (LeftParenthese:tokens') -> let (expr, rest) = parseExpression tokens'
                                                                        in case rest of
                                                                            (RightParenthese:LeftBrace:xs) -> let ((tree, rest'), _) = runWriter $ parseBlock xs 
                                                                                                              in (Node (Statement (StmtIf expr tree)) [], rest')
                                                                            (RightParenthese:xs) -> let (expr, rest') = parseExpression xs
                                                                                                    in (Node (Statement (StmtIf expr (Node (Statement (StatementExpr expr)) []))) [], rest')
                                                                            _ -> error "Syntax error: failed to parse statement - failed to find match parenthese in the if statement"
                                            _ -> error "Syntax error: failed to parse statement - no parenthese after 'if' keyword"
                          | t == While   = case tokens of
                                            (LeftParenthese:tokens') -> let (expr, rest) = parseExpression tokens'
                                                                        in case rest of
                                                                            (RightParenthese:LeftBrace:xs) -> let ((tree, rest'), _) = runWriter $ parseBlock xs 
                                                                                                              in (Node (Statement (StmtWhile expr tree)) [], rest')
                                                                            (RightParenthese:xs) -> let (expr, rest') = parseExpression xs
                                                                                                    in (Node (Statement (StmtWhile expr (Node (Statement (StatementExpr expr)) []))) [], rest')
                                                                            _ -> error "Syntax error: failed to parse statement - failed to find match parenthese in the 'while' statement"
                                            _ -> error "Syntax error: failed to parse statement - no parenthese after 'while' keyword"
                          | otherwise = let (expr, rest) = parseExpression (t:tokens)
                                        in case rest of
                                            (Semicolon:xs) -> (Node (Statement (StatementExpr expr)) [], xs)
                                            _              -> error "Syntax error: failed to parse statement"

parseBlock :: [Token] -> (Writer String (SyntaxTree, [Token]))
parseBlock tokens = do
    tell ("{\n")
    parse tokens (Node Block [])

parse :: [Token] -> SyntaxTree -> (Writer String (SyntaxTree, [Token]))
parse [] tree = return (tree, [])
parse (x:xs) (Node root childs) = case x of
                                   LeftBrace -> let
                                                    ((tree, rest), log) = runWriter $ parseBlock xs
                                                in do
                                                    tell log
                                                    parse rest (Node root (childs ++ [tree]))
                                   If        -> let
                                                 (tree, rest) = parseStatement (x:xs)
                                                in do
                                                    tell ""
                                                    parse rest (Node root (childs ++ [tree]))
                                   While     -> let
                                                 (tree, rest) = parseStatement (x:xs)
                                                in do
                                                    tell ""
                                                    parse rest (Node root (childs ++ [tree]))
                                   Id _      -> let
                                                 (tree, rest) = parseStatement (x:xs)
                                                in do
                                                    tell ""
                                                    parse rest (Node root (childs ++ [tree]))
                                   Num _     -> let
                                                 (tree, rest) = parseStatement (x:xs)
                                                in do
                                                    tell ""
                                                    parse rest (Node root (childs ++ [tree]))
                                   RightBrace -> do
                                                   tell ("}\n")
                                                   return ((Node root childs), xs)
                                   Type typ -> let
                                                   ((tree, rest), log) = runWriter $ parseDecl xs typ
                                               in do
                                                    tell log
                                                    parse rest (Node root (childs ++ [tree]))
                                   _ -> parse xs (Node root childs)

parseProgram :: String -> String
parseProgram str = snd $ runWriter $ parse (scan str) (Node Root [])

data ServiceInformation = ServiceInformation Int Int
    deriving (Show, Eq)
type SymbolTable = Map.Map Token Type

findSymbol :: [SymbolTable] -> Token -> Type
findSymbol [] token = error ("Syntax error: undefined symbol " ++ (show token))
findSymbol (x:xs) token = case (Map.lookup token x) of
                            Just m -> m
                            Nothing -> findSymbol xs token

translateList :: [SyntaxTree] -> ServiceInformation -> [SymbolTable] -> String -> (Writer String ([SymbolTable], ServiceInformation))
translateList [] info symbols out = do
                            tell out
                            return (symbols, info)
translateList (x:xs) info symbols out = let
                                      ((outSymbols, info'), log) = runWriter $ translate x info symbols out
                                   in translateList xs info' outSymbols log

translateFactor :: ServiceInformation -> [SymbolTable] -> Factor -> (String, String, ServiceInformation)
translateFactor info symbols (FactorNum num)   = ("", show num, info)
translateFactor info symbols (FactorStr str)   = ("", (show $ findSymbol symbols (Id str)) ++ "::" ++ str, info)
translateFactor info symbols (FactorExpr expr) = translateExpression info symbols expr

translateTerm :: ServiceInformation -> [SymbolTable] -> Term -> (String, String, ServiceInformation)
-- translateTerm _ _ expr | traceShow (expr) False = undefined
translateTerm info symbols (TermFactor factor)   = translateFactor info symbols factor
translateTerm info symbols (TermMul factor term) = let (out, var, info') = translateFactor info symbols factor
                                                       (out', var', (ServiceInformation i j)) = translateTerm info' symbols term
                                                       varNew = "t" ++ (show (i + 1))
                                                   in (out ++ out' ++ varNew ++ " = " ++ var ++ " * " ++ (var') ++ "\n", varNew, ServiceInformation (i+1) j)

translateAdd :: ServiceInformation -> [SymbolTable] -> Add -> (String, String, ServiceInformation)
translateAdd info symbols (AddTerm term)     = translateTerm info symbols term
translateAdd info symbols (AddPlus term add) = let (out, var, info') = translateTerm info symbols term
                                                   (out', var', (ServiceInformation i j)) = translateAdd info' symbols add
                                                   varNew = "t" ++ (show (i + 1))
                                               in (out ++ out' ++ varNew ++ " = " ++ var ++ " + " ++ (var') ++ "\n", varNew, ServiceInformation (i+1) j)

translateRel :: ServiceInformation -> [SymbolTable] -> Rel -> (String, String, ServiceInformation)
translateRel info symbols (RelAdd add)        = translateAdd info symbols add
translateRel info symbols (RelLess add rel)   = let (out, var, info') = translateAdd info symbols add
                                                    (out', var', (ServiceInformation i j)) = translateRel info' symbols rel
                                                    varNew = "t" ++ (show (i + 1))
                                                in (out ++ out' ++ varNew ++ " = " ++ var ++ " < " ++ (var') ++ "\n", varNew, ServiceInformation (i+1) j)
translateRel info symbols (RelLessEq add rel) = let (out, var, info') = translateAdd info symbols add
                                                    (out', var', (ServiceInformation i j)) = translateRel info' symbols rel
                                                    varNew = "t" ++ (show (i + 1))
                                                in (out ++ out' ++ varNew ++ " = " ++ var ++ " <= " ++ (var') ++ "\n", varNew, ServiceInformation (i+1) j)

translateExpression :: ServiceInformation -> [SymbolTable] -> Expression -> (String, String, ServiceInformation)
translateExpression info symbols (ExprRel rel)         = translateRel info symbols rel
translateExpression info symbols (ExprAssign rel expr) = let (out, var, info') = translateRel info symbols rel
                                                             (out', var', info'') = translateExpression info' symbols expr
                                                         in (out ++ out' ++ var ++ " = " ++ (var') ++ "\n", var, info'')

translate :: SyntaxTree -> ServiceInformation -> [SymbolTable] -> String -> (Writer String ([SymbolTable], ServiceInformation))
translate Nil i _ _ = return ([], i)
translate (Node elem []) info (sym:symbols) out = case elem of
                                                (Declaration t name) -> case Map.lookup (Id name) sym of 
                                                                            (Just _) -> error ("Syntax error: multiple declaration of '" ++ name ++ "'")
                                                                            Nothing  -> do
                                                                                tell out
                                                                                return (((Map.insert (Id name) t sym):symbols), info)
                                                (Statement (StatementExpr expr))     -> let (out', _, inf) = translateExpression info (sym:symbols) expr in do
                                                    tell (out ++ out')
                                                    return ((sym:symbols), inf)
                                                (Statement (StmtIf expr stmt))       -> let
                                                        (out', var, inf) = translateExpression info (sym:symbols) expr
                                                        ((symbols', (ServiceInformation i j)), out'')              = runWriter $ translate stmt inf (sym:symbols) ""
                                                        label = "L" ++ (show j)
                                                    in do
                                                        tell (out ++ out' ++ "ifFalse " ++ var ++ " goto " ++ label ++ "\n" ++ out'' ++ label ++ ": ")
                                                        return (symbols', ServiceInformation i (j+1))
                                                (Statement (StmtWhile expr stmt))    -> let
                                                        (out', var, inf) = translateExpression info (sym:symbols) expr
                                                        ((symbols', (ServiceInformation i j)), out'')              = runWriter $ translate stmt inf (sym:symbols) ""
                                                        label = "L" ++ (show j)
                                                        labelLoop = "L" ++ (show (j+1))
                                                    in do
                                                        tell (out ++ out' ++ labelLoop ++ ": ifFalse " ++ var ++ " goto " ++ label ++ "\n" ++ out'' ++ "goto " ++ labelLoop ++ "\n" ++ label ++ ": ")
                                                        return (symbols', ServiceInformation i (j+2))
                                                Block                -> do
                                                    tell (out ++ "{}\n")
                                                    return ((sym:symbols), info)
                                                Root                 -> do
                                                    tell out
                                                    return ((sym:symbols), info)
translate (Node elem (child:childs)) info symbols out = case elem of
                                                    Block -> let
                                                                ((outSymbols, info'), log) = runWriter $ translateList (child:childs) info (Map.empty:symbols) ""
                                                             in do 
                                                                tell (out ++ log)
                                                                return (symbols, info')
                                                    Root  -> let
                                                                ((outSymbols, info'), log) = runWriter $ translateList (child:childs) info (symbols) out
                                                             in do 
                                                                tell log
                                                                return (outSymbols, info')
                                                    _     -> error "Syntax error: unexpected syntax element with childs"

translatePretty str = hPutStrLn stdout $ snd $ runWriter $ translate (fst $ fst $ runWriter $ parse (scan $ str) (Node Root [])) (ServiceInformation 0 0) [] ""

data Terminal = Ch Char | Eol
  deriving (Show, Eq, Ord)
data NonTerminal = NT String
  deriving (Show, Eq, Ord)
type GrammarString = [Either Terminal NonTerminal]

type SyntaxTable = Map (NonTerminal, Terminal) GrammarString

lc = Left . Ch
rnt = Right . NT

handleState :: SyntaxTable -> State (GrammarString, String) ()
handleState table = do
                      (stack', input') <- get
                      let (top:stack) = stack'
                      case top of
                        Left (Ch c) -> do let (current:input) = input'
                                          if c == current
                                          then put (stack, input)
                                          else error ("Syntax error 1: Stack - " ++ (show stack') ++ "; Input - " ++ (show input'))
                        Right nt    -> let current = if input' == "" then Eol else Ch $ head input'
                                       in case Map.lookup (nt, current) table of
                                            Just res -> do put (res ++ stack, input')
                                            Nothing  -> error ("Syntax error 2: Stack - " ++ (show stack') ++ "; Input - " ++ (show input'))

parseTableParser :: SyntaxTable -> State (GrammarString, String) ()
parseTableParser table = do handleState table
                            (stack, str) <- get
                            if stack == [Left Eol]
                            then if str == [] then return () else error "Syntax error 3"
                            else parseTableParser table

parse' table start str = snd $ runState (parseTableParser table) ([Right start, Left Eol], str)

-- S -> 0 S 1 | 0 1
parse422a :: String -> (GrammarString, String)
parse422a str = let table = Map.fromList [
                        ((NT "S", Ch '0'), [lc '0', rnt "S'"]),
                        ((NT "S'", Ch '0'), [rnt "S", lc '1']),
                        ((NT "S'", Ch '1'), [lc '1'])
                      ]
                    start = NT "S"
                in
                  parse' table start str

-- S -> S S + | S S * | a
parse421 :: String -> (GrammarString, String)
parse421 str = let table = Map.fromList [
                        ((NT "S", Ch 'a'), [lc 'a', rnt "S1", rnt "S3"]),
                        ((NT "S1", Ch 'a'), [lc 'a', rnt "S2"]),
                        ((NT "S1", Eol), []),
                        ((NT "S1", Ch '+'), []),
                        ((NT "S1", Ch '*'), []),
                        ((NT "S2", Ch '+'), [lc '+']),
                        ((NT "S2", Ch '*'), [lc '*']),
                        ((NT "S3", Ch '+'), []),
                        ((NT "S3", Ch '*'), []),
                        ((NT "S3", Ch 'a'), map (\x -> rnt x) ["S", "S2", "S3"]),
                        ((NT "S3", Eol), [])
                      ]
                   start = NT "S"
                in
                  parse' table start str

-- S -> + S S | * S S | a
parse422b :: String -> (GrammarString, String)
parse422b str = let table = Map.fromList [
                        ((NT "S", Ch 'a'), [lc 'a']),
                        ((NT "S", Ch '+'), [lc '+', rnt "S", rnt "S"]),
                        ((NT "S", Ch '*'), [lc '*', rnt "S", rnt "S"])
                      ]
                    start = NT "S"
                in
                  parse' table start str

-- S -> S (S) S | Eol
parse422v :: String -> (GrammarString, String)
parse422v str = let table = Map.fromList [
                        ((NT "S", Ch '('), [lc '(', rnt "S", lc ')', rnt "S", rnt "S"]),
                        ((NT "S", Ch ')'), []),
                        ((NT "S", Eol), [])
                      ]
                    start = NT "S"
                in
                  parse' table start str

-- S -> S + S | S S | (S) | S* | a
parse422g :: String -> (GrammarString, String)
parse422g str = let table = Map.fromList [
                        ((NT "S", Ch '('), [lc '(', rnt "S", lc ')', rnt "S'"]),
                        ((NT "S", Ch 'a'), [lc 'a', rnt "S'"]),
                        ((NT "S'", Ch 'a'), [lc 'a', rnt "S'"]),
                        ((NT "S'", Ch '('), [lc '(', rnt "S", lc ')', rnt "S'"]),
                        ((NT "S'", Ch ')'), []),
                        ((NT "S'", Ch '+'), [lc '+', rnt "S", rnt "S'"]),
                        ((NT "S'", Ch '*'), [lc '*', rnt "S'"]),
                        ((NT "S'", Eol), [])
                      ]
                    start = NT "S"
                in
                  parse' table start str

-- S -> (L) | a 
-- L -> L,S | S
parse422d :: String -> (GrammarString, String)
parse422d str = let table = Map.fromList [
                        ((NT "S", Ch 'a'), [lc 'a']),
                        ((NT "S", Ch '('), [lc '(', rnt "S", rnt "L", lc ')']),
                        ((NT "L", Ch ')'), []),
                        ((NT "L", Eol), []),
                        ((NT "L", Ch ','), [lc ',', rnt "S", rnt "L"])
                      ]
                    start = NT "S"
                in
                  parse' table start str

parseIf :: String -> (GrammarString, String)
parseIf str = let table = Map.fromList [
                        ((NT "stmt", Ch 'i'), [lc 'i', rnt "stmt", rnt "stmtTail"]),
                        ((NT "stmt", Ch 'w'), [lc 'w', rnt "stmt"]),
                        ((NT "stmt", Ch '{'), [lc '{', rnt "list", lc '}']),
                        ((NT "stmt", Ch 's'), [lc 's']),
                        ((NT "stmtTail", Ch 'e'), [lc 'e', rnt "stmt"]),
                        ((NT "stmtTail", Eol), []),
                        ((NT "stmtTail", Ch '}'), []),
                        ((NT "list", Ch 'i'), [rnt "stmt", rnt "listTail"]),
                        ((NT "list", Ch 'w'), [rnt "stmt", rnt "listTail"]),
                        ((NT "list", Ch '{'), [rnt "stmt", rnt "listTail"]),
                        ((NT "list", Ch 's'), [rnt "stmt", rnt "listTail"]),
                        ((NT "listTail", Ch 'i'), [rnt "list"]),
                        ((NT "listTail", Ch 'w'), [rnt "list"]),
                        ((NT "listTail", Ch '{'), [rnt "list"]),
                        ((NT "listTail", Ch 's'), [rnt "list"]),
                        ((NT "listTail", Eol), []),
                        ((NT "listTail", Ch '}'), [])
                      ]
                  start = NT "stmt"
                in
                  parse' table start str