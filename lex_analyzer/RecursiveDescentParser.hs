module RecursiveDescentParser where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Debug.Trace
import Control.Monad.State

data Token = Plus | Minus | Div | Mul 
            | Num Int | NumFloat Double | Id String
            | More | Less | MoreEq | LessEq | Equal | NotEqual
    deriving (Show, Eq)

extractDigit :: String -> Int -> (Int, String)
extractDigit [] acc = (acc, [])
extractDigit (x:xs) acc | isDigit x = extractDigit xs (acc*10 + (digitToInt x))
                        | isLetter x = error "Syntax error: number before letter"
                        | otherwise = (acc, (x:xs))

extractWord :: String -> String -> (String, String)
extractWord [] acc = (acc, [])
extractWord (x:xs) acc | isLetter x || isDigit x = extractWord xs (acc ++ [x])
                       | otherwise = (acc, (x:xs))

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
                   | x == '+'                           = tokens ++ [Plus] ++ scanInternal xs []
                   | x == '-'                           = tokens ++ [Minus] ++ scanInternal xs []
                   | isDigit x                          = let (digit, rest) = extractDigit xs (digitToInt x)
                                                             in tokens ++ [Num digit] ++ scanInternal rest []
                   | isLetter x                         = let (word, rest) = extractWord xs [x]
                                                             in tokens ++ [Id word] ++ scanInternal rest []
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

parseInternal1 :: String -> Int -> (String, Bool)
parseInternal1 [] (-1) = ([], True)
parseInternal1 [] _ = ([], False)
parseInternal1 (x:xs) n | x == '+' = let (str, res) = parseInternal1 xs (n+1) in
                    if res then parseInternal1 str (n+1) else (str, False)
               | x == '-' = let (str, res) = parseInternal1 xs (n+1) in
                    if res then parseInternal1 str (n+1) else (str, False)
               | x == 'a' = (xs, True)
               | otherwise = ((x:xs), False)

parseInternal2 :: String -> Int -> (String, Bool)
parseInternal2 [] (-1) = ([], True)
parseInternal2 [] 0 = ([], True)
parseInternal2 [] _ = ([], False)
parseInternal2 (x:xs) n
               | x == '(' = let ((str), res) = parseInternal2 xs (n+1) in
                    if res && (str /= []) && (head str == ')') then parseInternal2 (tail str) n else (str, False)
               | otherwise = ((x:xs), True)

parseInternal3 :: String -> Int -> (String, Bool)
parseInternal3 [] (-1) = ([], True)
parseInternal3 [] 0 = ([], False)
parseInternal3 [] _ = ([], False)
parseInternal3 (x:xs) n
               | x == '0' && length xs /= 0 && (head xs) == '1' = (tail xs, True)
               | x == '0' = let (str, res) = parseInternal3 xs (n+1) in
                    if res && (str /= []) && (head str == '1') then 
                        if tail str == [] && n == 0 then ([], True) else parseInternal3 (tail str) n
                    else (str, False)
               | otherwise = ((x:xs), True)

parse :: String -> Maybe String
parse str = let (str', res) = parseInternal3 str 0 in
    if res && (str' == []) then Nothing else Just ("Syntax error around " ++ str' ++ ".")