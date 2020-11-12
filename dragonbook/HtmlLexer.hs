module LexAnalyzer where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Word
import Debug.Trace
import Control.Monad.State

data TagInfo = TagInfo String [(String, String)]
    deriving (Show, Eq)
    
data Token = Literal String | OpenTag TagInfo | CloseTag TagInfo
    deriving (Show, Eq)

extractTagAttr :: String -> (Maybe (String, String), String)
-- extractTagAttr ll | traceShow (ll) False = undefined
extractTagAttr i =       let
                            s        = dropWhile (==' ') i
                            ab = elemIndices ' ' s
                         in if (ab == []) then (Nothing, s) else case drop (head ab) s of
                                                                (' ':'=':' ':v) -> let
                                                                                    spaceIndices = elemIndices ' ' $ dropWhile (==' ') v
                                                                                    brIndices = elemIndices '>' $ dropWhile (==' ') v
                                                                                    pos = if (spaceIndices == []) then head $ brIndices else head $ spaceIndices
                                                                                    (val, r) = splitAt pos v
                                                                                   in (Just (take (head ab) s, val), r)
                                                                _ -> error "Failed to parse tag attribute"


extractTagInfo :: String -> [(String, String)] -> ([(String, String)], String)
extractTagInfo [] _ = error "FAIL"
extractTagInfo (x:xs) acc | x == '>' = (acc, xs)
                          | otherwise = let (p, rest) = extractTagAttr (x:xs)
                                        in case p of 
                                            (Just p) -> extractTagInfo rest (acc ++ [p])
                                            Nothing  -> extractTagInfo rest acc


extractCloseTag :: String -> String -> (Token, String)
extractCloseTag [] acc = error "Lexical error - failed to find matched symbol '>' for '<'"
extractCloseTag (x:xs) acc | x == '>' = (CloseTag (TagInfo acc []), xs)
                           | x == ' ' = let (taginfo, rest) = (extractTagInfo xs [])
                                        in ((CloseTag (TagInfo acc taginfo)), rest)
                           | otherwise = extractCloseTag xs (acc ++ [x])

extractOpenTag :: String -> String -> (Token, String)
extractOpenTag [] acc = error "Lexical error - failed to find matched symbol '>' for '<'"
extractOpenTag (x:xs) acc  | x == '>' = (OpenTag (TagInfo acc []), xs)
                           | x == ' ' = let (taginfo, rest) = (extractTagInfo xs [])
                                        in ((OpenTag (TagInfo acc taginfo)), rest)
                           | otherwise = extractOpenTag xs (acc ++ [x])

scanInternal :: String -> String -> [Token] -> [Token]
scanInternal [] acc tokens                             = tokens ++ (if acc /= [] then [Literal acc] else [])
scanInternal (k:[]) acc tokens                         = scanInternal "" (k:acc) tokens
scanInternal (x:x':xs) acc tokens
                   | x == '<' && x' == '/'                          = let (closeTag, rest) = extractCloseTag xs ""
                                                                      in tokens ++ [Literal acc] ++ [closeTag] ++ scanInternal (rest) "" []
                   | x == '<'                                       = let (openTag, rest) = extractOpenTag (x':xs) ""
                                                                      in tokens ++ (if acc /= [] then [Literal acc] else []) ++ [openTag] ++ scanInternal (rest) "" []
                   | otherwise                          = scanInternal xs (acc ++ (x:[x'])) tokens

scanHtml = \x -> scanInternal x "" []



