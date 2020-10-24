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
            | More | Less | MoreEq | LessEq | Equal | NotEqual | LeftBrace | RightBrace | Semicolon
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
                   | otherwise                          = error "Syntax error: unexpected symbol"

scan str = scanInternal str []

data SyntaxElement = Root | Block | Statement String | Declaration Type String
    deriving (Show, Eq)

data SyntaxTree = Nil | Node SyntaxElement [(SyntaxTree)]
    deriving (Eq, Show)

type SymbolTable = Map.Map Token Type

findSymbol :: [SymbolTable] -> Token -> Type
findSymbol [] token = error ("Syntax error: undefined symbol " ++ (show token))
findSymbol (x:xs) token = case (Map.lookup token x) of
                            Just m -> m
                            Nothing -> findSymbol xs token

parseDecl :: [Token] -> Type -> [SymbolTable] -> ([SymbolTable], (Writer String (SyntaxTree, [Token])))
parseDecl tokens typ (s:symbols) = case tokens of
                   ((Id name):Semicolon:xs) -> (((Map.insert (Id name) typ s):symbols), (return (Node (Declaration typ name) [], xs)))
                   _         -> error "Syntax error: failed to parse declaration"

parseStatement :: [Token] -> String -> [SymbolTable] -> (Writer String (SyntaxTree, [Token]))
parseStatement tokens name symbols = case tokens of
                   (Semicolon:xs) -> do
                        tell ((concat  $ take (length symbols) $ repeat "    ") ++ show (findSymbol symbols (Id name)) ++ "::" ++ name ++ ";\n")
                        return (Node (Statement name) [], xs)
                   _         -> error "Syntax error: failed to parse statement"

parseBlock :: [Token] -> [SymbolTable] -> (Writer String (SyntaxTree, [Token]))
parseBlock tokens symbols = do
    tell ((concat  $ take (length symbols) $ repeat "    ") ++ "{\n")
    parse tokens (Node Block []) (Map.empty:symbols)

parse :: [Token] -> SyntaxTree -> [SymbolTable] -> (Writer String (SyntaxTree, [Token]))
parse [] tree _ = return (tree, [])
parse (x:xs) (Node root childs) symbols = case x of
                                   LeftBrace -> let
                                                    ((tree, rest), log) = runWriter $ parseBlock xs symbols
                                                in do
                                                    tell log
                                                    parse rest (Node root (childs ++ [tree])) symbols
                                   Id str -> let
                                                 ((tree, rest), log) = runWriter $ parseStatement xs str symbols
                                             in do
                                                tell log
                                                parse rest (Node root (childs ++ [tree])) symbols
                                   RightBrace -> do
                                                   tell ((concat  $ take ((length symbols) - 1)  $ repeat "    ") ++ "}\n")
                                                   return ((Node root childs), xs)
                                   Type typ -> let
                                                   (symTable, wr) = parseDecl xs typ symbols
                                                   ((tree, rest), log) = runWriter $ wr
                                               in do
                                                tell log
                                                parse rest (Node root (childs ++ [tree])) symTable
                                   _ -> parse xs (Node root childs) symbols

parseProgram :: String -> String
parseProgram str = snd $ runWriter $ parse (scan str) (Node Root []) []

translateList :: [SyntaxTree] -> [SymbolTable] -> String -> (Writer String [SymbolTable])
translateList [] symbols out = do
                            tell out
                            return symbols
translateList (x:xs) symbols out = let
                                      (outSymbols, log) = runWriter $ translate x symbols out
                                   in translateList xs outSymbols log

translate :: SyntaxTree -> [SymbolTable] -> String -> (Writer String [SymbolTable])
translate Nil _ _ = return []
translate (Node elem []) (sym:symbols) out = case elem of
                                                (Declaration t name) -> do
                                                    tell out
                                                    return ((Map.insert (Id name) t sym):symbols)
                                                (Statement stmt)     -> do
                                                    tell (out ++ ((concat  $ take ((length symbols) + 1) $ repeat "    ") ++ show (findSymbol (sym:symbols) (Id stmt)) ++ "::" ++ stmt ++ ";\n"))
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

translatePretty str = hPutStrLn stdout $ snd $ runWriter $ translate (fst $ fst $ runWriter $ parse (scan $ str) (Node Root []) []) [] ""

