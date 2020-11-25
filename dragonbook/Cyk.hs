module Cyk where

import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

extractElem :: Eq a => (a -> Bool) -> [a] -> (Maybe a, [a])
extractElem f l = extractElem' f l []
                            where extractElem' f (x:xs) acc = if f x 
                                                              then (Just x, acc ++ xs)
                                                              else extractElem' f xs (acc ++ [x])
                                  extractElem' _ [] acc = (Nothing, acc)

data Terminal = T Char | E
    deriving (Show, Eq, Ord)

data NonTerminal = NT String
    deriving (Show, Eq, Ord)

type Grammar = [(NonTerminal, [Either Terminal NonTerminal])]

parseGrammarString :: [String] -> String -> [[Either Terminal NonTerminal]] -> [[Either Terminal NonTerminal]]
parseGrammarString nts [] acc     = acc
parseGrammarString nts str@(x:xs) acc =     if x == '|'
                                            then parseGrammarString nts xs ([]:acc)
                                            else let (newElem, offset) = case find (\a -> isPrefixOf a str) nts of
                                                                            Nothing -> if x == '$' then (Left E, 1) else (Left  $ T x, 1)
                                                                            Just nt -> (Right $ NT nt, length nt)
                                                 in parseGrammarString nts (drop offset str) (if acc == [] then [[newElem]] else (head acc ++ [newElem]): tail acc)

parseGrammar :: [String] -> Grammar
parseGrammar strs = let woSpaces = map (\s -> filter (/= ' ') s) strs
                        idxs = map (\s -> fromJust . findIndex (== '-') $ s) woSpaces
                        tuples = map (\(s, idx) -> splitAt idx s) (zip woSpaces idxs)
                        nonTerminals = map fst tuples
                        grammarStrings = map (tail . tail . snd) tuples
                        parsed = map (\(nt, s) -> map (\y -> (NT nt, y)) $ parseGrammarString nonTerminals s []) (zip nonTerminals grammarStrings)
                    in concat parsed

toEFreeGrammar :: Grammar -> Grammar
toEFreeGrammar g = case extractElem (\(nt, grStr) -> if grStr == [Left E] then True else False) g of
                    (Nothing, _) -> rmdups g
                    (Just (nt, _), g') -> toEFreeGrammar (g' ++ foldr (\(nt', grStr') a -> if elem (Right nt) grStr' then a ++ [(nt', filter (/= Right nt) grStr')] else a) [] g')

removeSingleProductions :: Grammar -> Grammar
removeSingleProductions g = case extractSingleProduction g of
                                (Nothing, _) -> g
                                (Just (ntToRemove, ntReplacement), g')
                                    -> removeSingleProductions $ map (\(nt, grStr) -> 
                                                                        (if nt == ntToRemove then ntReplacement else nt, map (\x ->
                                                                            if x == Right ntToRemove then Right ntReplacement else x
                                                                        ) grStr)
                                                                        ) g'
                            where extractSingleProduction g = extractSingleProduction' g []
                                    where extractSingleProduction' (x@(nt, grStr):xs) acc = case grStr of
                                                                                            [Right nt'] -> (Just (nt, nt'), acc ++ xs)
                                                                                            _ -> extractSingleProduction' xs (acc ++ [x])
                                          extractSingleProduction' [] acc = (Nothing, acc)
