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
            | Num Int | Id String | Type Type
            | More | Less | MoreEq | LessEq | Equal | NotEqual | LeftBrace | RightBrace | Semicolon | Assign | LeftParenthese | RightParenthese
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
                                  _      -> (Id acc, [])
extractWord (x:xs) acc | isLetter x || isDigit x = extractWord xs (acc ++ [x])
                       | otherwise = case acc of  "bool" -> (Type Boolean, (x:xs))
                                                  "int"  -> (Type Integ, (x:xs))
                                                  "char" -> (Type Character, (x:xs))
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

data Statement = StatementExpr Expression | If Expression Statement | While Expression Statement | DoWhile Statement Expression
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
parseStatement tokens = let (expr, rest) = parseExpression tokens
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


type SymbolTable = Map.Map Token Type

findSymbol :: [SymbolTable] -> Token -> Type
findSymbol [] token = error ("Syntax error: undefined symbol " ++ (show token))
findSymbol (x:xs) token = case (Map.lookup token x) of
                            Just m -> m
                            Nothing -> findSymbol xs token

translateList :: [SyntaxTree] -> [SymbolTable] -> String -> (Writer String [SymbolTable])
translateList [] symbols out = do
                            tell out
                            return symbols
translateList (x:xs) symbols out = let
                                      (outSymbols, log) = runWriter $ translate x symbols out
                                   in translateList xs outSymbols log

translateFactor :: [SymbolTable] -> Factor -> String
translateFactor symbols (FactorNum num)   = show num
translateFactor symbols (FactorStr str)   = (show $ findSymbol symbols (Id str)) ++ "::" ++ str
translateFactor symbols (FactorExpr expr) = "(" ++ (translateExpression symbols expr) ++ ")"

translateTerm :: [SymbolTable] -> Term -> String
translateTerm symbols (TermFactor factor)   = translateFactor symbols factor
translateTerm symbols (TermMul factor term) = translateFactor symbols factor ++ " * " ++ translateTerm symbols term

translateAdd :: [SymbolTable] -> Add -> String
translateAdd symbols (AddTerm term)     = translateTerm symbols term
translateAdd symbols (AddPlus term add) = translateTerm symbols term ++ " + " ++ translateAdd symbols add

translateRel :: [SymbolTable] -> Rel -> String
translateRel symbols (RelAdd add)        = translateAdd symbols add
translateRel symbols (RelLess add rel)   = translateAdd symbols add ++ " < " ++ translateRel symbols rel
translateRel symbols (RelLessEq add rel) = translateAdd symbols add ++ " <= " ++ translateRel symbols rel

translateExpression :: [SymbolTable] -> Expression -> String
translateExpression symbols (ExprRel rel)         = translateRel symbols rel
translateExpression symbols (ExprAssign rel expr) = translateRel symbols rel ++ " = " ++ (translateExpression symbols expr)

translate :: SyntaxTree -> [SymbolTable] -> String -> (Writer String [SymbolTable])
translate Nil _ _ = return []
translate (Node elem []) (sym:symbols) out = case elem of
                                                (Declaration t name) -> do
                                                    tell out
                                                    return ((Map.insert (Id name) t sym):symbols)
                                                (Statement (StatementExpr expr))     -> do
                                                    tell (out ++ (concat  $ take ((length symbols) + 1) $ repeat "    ") ++ (translateExpression (sym:symbols) expr) ++ ";\n")
                                                    return (sym:symbols)
                                                Block                -> do
                                                    tell (out ++ (concat  $ take (length (sym:symbols)) $ repeat "    ") ++ "{}\n")
                                                    return (sym:symbols)
                                                Root                 -> do
                                                    tell out
                                                    return (sym:symbols)
translate (Node elem (child:childs)) symbols out = case elem of
                                                    Block -> let
                                                                (outSymbols, log) = runWriter $ translateList (child:childs) (Map.empty:symbols) ""
                                                             in do 
                                                                tell (out ++ (concat  $ take (length symbols) $ repeat "    ") ++ "{\n" ++ log ++ (concat  $ take (length symbols) $ repeat "    ") ++ "}\n")
                                                                return symbols
                                                    Root  -> let
                                                                (outSymbols, log) = runWriter $ translateList (child:childs) (symbols) out
                                                             in do 
                                                                tell log
                                                                return outSymbols
                                                    _     -> error "Syntax error: unexpected syntax element with childs"

translatePretty str = hPutStrLn stdout $ snd $ runWriter $ translate (fst $ fst $ runWriter $ parse (scan $ str) (Node Root [])) [] ""

