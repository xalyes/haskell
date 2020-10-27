module RecursiveDescentParser where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Debug.Trace
import Control.Monad.Writer
import System.IO
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
            | Num Int | Id String | Type Type | If
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
                                  _      -> (Id acc, [])
extractWord (x:xs) acc | isLetter x || isDigit x = extractWord xs (acc ++ [x])
                       | otherwise = case acc of  "bool" -> (Type Boolean, (x:xs))
                                                  "int"  -> (Type Integ, (x:xs))
                                                  "char" -> (Type Character, (x:xs))
                                                  "if"   -> (If, (x:xs))
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

data Statement = StatementExpr Expression | StmtIf Expression SyntaxTree | While Expression Statement | DoWhile Statement Expression
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

data ServiceInformation = ServiceInformation Int
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
                                                       (out', var', (ServiceInformation i)) = translateTerm info' symbols term
                                                       varNew = "t" ++ (show (i + 1))
                                                   in (out ++ out' ++ varNew ++ " = " ++ var ++ " * " ++ (var') ++ "\n", varNew, ServiceInformation (i+1))

translateAdd :: ServiceInformation -> [SymbolTable] -> Add -> (String, String, ServiceInformation)
translateAdd info symbols (AddTerm term)     = translateTerm info symbols term
translateAdd info symbols (AddPlus term add) = let (out, var, info') = translateTerm info symbols term
                                                   (out', var', (ServiceInformation i)) = translateAdd info' symbols add
                                                   varNew = "t" ++ (show (i + 1))
                                               in (out ++ out' ++ varNew ++ " = " ++ var ++ " + " ++ (var') ++ "\n", varNew, ServiceInformation (i+1))

translateRel :: ServiceInformation -> [SymbolTable] -> Rel -> (String, String, ServiceInformation)
translateRel info symbols (RelAdd add)        = translateAdd info symbols add
translateRel info symbols (RelLess add rel)   = let (out, var, info') = translateAdd info symbols add
                                                    (out', var', (ServiceInformation i)) = translateRel info' symbols rel
                                                    varNew = "t" ++ (show (i + 1))
                                                in (out ++ out' ++ varNew ++ " = " ++ var ++ " < " ++ (var') ++ "\n", varNew, ServiceInformation (i+1))
translateRel info symbols (RelLessEq add rel) = let (out, var, info') = translateAdd info symbols add
                                                    (out', var', (ServiceInformation i)) = translateRel info' symbols rel
                                                    varNew = "t" ++ (show (i + 1))
                                                in (out ++ out' ++ varNew ++ " = " ++ var ++ " <= " ++ (var') ++ "\n", varNew, ServiceInformation (i+1))

translateExpression :: ServiceInformation -> [SymbolTable] -> Expression -> (String, String, ServiceInformation)
translateExpression info symbols (ExprRel rel)         = translateRel info symbols rel
translateExpression info symbols (ExprAssign rel expr) = let (out, var, info') = translateRel info symbols rel
                                                             (out', var', info'') = translateExpression info' symbols expr
                                                         in (out ++ out' ++ var ++ " = " ++ (var') ++ "\n", var, info'')

translate :: SyntaxTree -> ServiceInformation -> [SymbolTable] -> String -> (Writer String ([SymbolTable], ServiceInformation))
translate Nil i _ _ = return ([], i)
translate (Node elem []) info (sym:symbols) out = case elem of
                                                (Declaration t name) -> do
                                                    tell out
                                                    return (((Map.insert (Id name) t sym):symbols), info)
                                                (Statement (StatementExpr expr))     -> let (out', _, inf) = translateExpression info (sym:symbols) expr in do
                                                    tell (out ++ out')
                                                    return ((sym:symbols), inf)
                                                (Statement (StmtIf expr stmt))       -> let
                                                        (out', var, inf) = translateExpression info (sym:symbols) expr
                                                        ((symbols', inf'), out'')              = runWriter $ translate stmt inf (sym:symbols) ""
                                                    in do
                                                        tell (out ++ out' ++ "ifFalse " ++ var ++ " goto L\n" ++ out'' ++ "L: ")
                                                        return (symbols', inf')
                                                Block                -> do
                                                    tell (out ++ (concat  $ take (length (sym:symbols)) $ repeat "    ") ++ "{}\n")
                                                    return ((sym:symbols), info)
                                                Root                 -> do
                                                    tell out
                                                    return ((sym:symbols), info)
translate (Node elem (child:childs)) info symbols out = case elem of
                                                    Block -> let
                                                                ((outSymbols, info'), log) = runWriter $ translateList (child:childs) info (Map.empty:symbols) ""
                                                             in do 
                                                                tell (out ++ (concat  $ take (length symbols) $ repeat "    ") ++ "{\n" ++ log ++ (concat  $ take (length symbols) $ repeat "    ") ++ "}\n")
                                                                return (symbols, info')
                                                    Root  -> let
                                                                ((outSymbols, info'), log) = runWriter $ translateList (child:childs) info (symbols) out
                                                             in do 
                                                                tell log
                                                                return (outSymbols, info')
                                                    _     -> error "Syntax error: unexpected syntax element with childs"

translatePretty str = hPutStrLn stdout $ snd $ runWriter $ translate (fst $ fst $ runWriter $ parse (scan $ str) (Node Root [])) (ServiceInformation 0) [] ""

