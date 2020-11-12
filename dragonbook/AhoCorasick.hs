module AhoCorasick where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Word
import Data.Maybe
import Debug.Trace
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map

type Trie = [TrieNode]

type TrieNode = Map.Map Char Int

modifyElem :: Eq a => Int -> [a] -> a -> [a]
modifyElem idx l new = let tmp = Data.List.splitAt (idx+1) l
                       in if (fst tmp) == [] then [new] ++ (snd tmp) else (init . fst $ tmp) ++ [new] ++ (snd tmp)

addWord :: String -> Int -> Trie -> Trie
addWord [] _ trie = trie
addWord word idx trie = let
                            currentNode = trie !! idx
                            c = head word
                            nextNode = Map.lookup c currentNode
                        in case nextNode of
                            Just m -> addWord (tail word) m trie
                            Nothing -> addWord (tail word) (length trie) ((modifyElem idx trie (Map.insert c (length trie) currentNode)) ++ [Map.empty])

buildTrie :: [String] -> Trie
buildTrie words = foldl (\trie word -> addWord word 0 trie) [Map.empty] words

findPrefixState :: String -> Trie -> Int -> Int
findPrefixState [] _ st = st
findPrefixState word trie st = let c = head word
                                   m = trie !! st
                                   try = Map.lookup c m
                               in case try of
                                    Just newState -> findPrefixState (tail word) trie newState
                                    Nothing -> findPrefixState (tail word) trie st

prefixes :: Trie -> Int -> String -> [(Int, Int)]
prefixes trie s stack = if stack == []
                        then Map.foldrWithKey (\k v arr -> arr ++ (prefixes trie v (stack ++ [k]))) [] (trie !! s)
                        else((s, findPrefixState (tail stack) trie 0): Map.foldrWithKey (\k v arr -> arr ++ (prefixes trie v (stack ++ [k]))) [] (trie !! s) )

buildTrieAndPrefixTable :: [String] -> (Trie, Map.Map Int Int)
buildTrieAndPrefixTable words = let trie = buildTrie words
                                in (trie, Map.fromList $ prefixes trie 0 "")