module RecursiveDescentParser where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Debug.Trace
import Control.Monad.State

data Type = Boolean | Integ | Character
    deriving (Show, Eq)

data Token = Plus | Minus | Div | Mul 
            | Num Int | Id String | Type Type
            | More | Less | MoreEq | LessEq | Equal | NotEqual | LeftBrace | RightBrace | Semicolon
    deriving (Show, Eq)

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

{-
showNode len (Node root childs) acc = let
        value = show root
        filler = concat $ take ((len - (length value)) `div` 2) $ repeat " "
    in acc ++ filler ++ value ++ filler

show' :: Int -> SyntaxTree -> String -> String
show' len (Node root childs) acc = let 
        childsCount = length childs
    in acc ++ foldr (show' (len `div` childsCount)) (foldr (showNode (len `div` childsCount)) "" childs) childs

instance Show SyntaxTree where
    show tree = show' 300 tree ""
-}

parseType :: [Token] -> Type -> (SyntaxTree, [Token])
parseType tokens typ = case tokens of
                   ((Id name):Semicolon:xs) -> (Node (Declaration typ name) [], xs)
                   _         -> error "Syntax error: failed to parse type"

parseStatement :: [Token] -> String -> (SyntaxTree, [Token])
parseStatement tokens name = case tokens of
                   (Semicolon:xs) -> (Node (Statement name) [], xs)
                   _         -> error "Syntax error: failed to parse statement"

parse :: [Token] -> SyntaxTree -> (SyntaxTree, [Token])
parse [] tree = (tree, [])
parse (x:xs) (Node root childs) = case x of
                                   LeftBrace -> let
                                                    (tree, rest) = parse xs (Node Block [])
                                                in parse rest (Node root (childs ++ [tree]))
                                   Id str -> let
                                                 (tree, rest) = parseStatement xs str
                                             in parse rest (Node root (childs ++ [tree]))
                                   RightBrace -> ((Node root childs), xs)
                                   Type typ -> let
                                                   (tree, rest) = parseType xs typ
                                               in parse rest (Node root (childs ++ [tree]))
                                   _ -> parse xs (Node root childs)



