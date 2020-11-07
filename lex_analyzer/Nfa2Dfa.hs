module Nfa2Dfa where

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

import Data.Map (Map)
import qualified Data.Map as Map

data Symbol = Empty | Char Char
    deriving (Show, Eq)

type Edge = (Symbol, Int)
type Fsm = ([Char], [[Edge]], Int, [Int])

type NfaStates = [Int]
type NfaDfaStatesTable = Map NfaStates Int

modifyElem :: Eq a => Int -> [a] -> a -> [a]
modifyElem idx l new = let tmp = Data.List.splitAt (idx+1) l
                       in if (fst tmp) == [] then [new] ++ (snd tmp) else (init . fst $ tmp) ++ [new] ++ (snd tmp)

findByValue :: (Eq a, Eq b) => Map a b -> b -> a
findByValue m val = fst $ fromJust $ find (\(k, v) -> val == v) (Map.toList m)

getTransitions :: [Edge] -> Symbol -> [Int]
getTransitions edges sym = map (snd) . filter (\(sym', s) -> sym' == sym) $ edges

eClosure :: [[Edge]] -> [Int] -> [Int]
eClosure fsm states = let tmp = filter (\s -> not (elem s states)) . nub . concat . map (\s -> getTransitions (fsm !! s) Empty) $ states
                      in if tmp == [] then states else eClosure fsm (tmp ++ states)

move :: [[Edge]] -> [Int] -> Char -> [Int]
-- move _ t a | traceShow (t, a) False = undefined
move fsm t a = nub . concat . map (\s -> getTransitions (fsm !! s) (Char a)) $ t

getNewState :: [[Edge]] -> NfaDfaStatesTable -> NfaStates -> Char -> (NfaDfaStatesTable, Int)
getNewState fsm table nfaStates a | traceShow (nfaStates) False = undefined
getNewState fsm table nfaStates a = let newNfaStates = eClosure fsm (move fsm nfaStates a)
                                        newDfaState = (Map.foldr max 0 table) + 1
                                        newTable = Map.insertWith (\newVal oldVal -> oldVal) newNfaStates newDfaState table
                                    in if newNfaStates == [] then (table, -1) else (newTable, newTable Map.! newNfaStates)

addTransition :: [[Edge]] -> Int -> Char -> Int -> [[Edge]]
addTransition fsm t a u = modifyElem t fsm (fsm !! t ++ [(Char a, u)])

getNewDTransition nfa a table t dStates dfa | traceShow (t) False = undefined
getNewDTransition nfa a table t dStates dfa = let (newTable, u) = getNewState nfa table (findByValue table t) a
                                                  newDStates = if elem u dStates then dStates else dStates ++ [u]
                                                  newDfa = addTransition dfa t a u
                                              in if u /= -1 then (newTable, newDStates, newDfa) else (table, dStates, dfa)

handleDfaState :: [[Edge]] -> NfaDfaStatesTable -> [Int] -> [Int] -> [Char] -> [[Edge]] -> (NfaDfaStatesTable, [[Edge]])
-- handleDfaState _ table dStates markedStates _ dfa | traceShow (dfa) False = undefined
handleDfaState nfa table dStates markedStates alph dfa | sort dStates == sort markedStates = (table, init dfa)
handleDfaState nfa table dStates markedStates alph dfa | otherwise               = let t = head $ dStates \\ markedStates
                                                                                       (newTable, newDStates, newDfa) = foldl (\(table', dStates', dfa') a -> getNewDTransition nfa a table' t dStates' dfa') (table, dStates, dfa) alph
                                                                                   in handleDfaState nfa newTable newDStates (t:markedStates) alph (newDfa ++ [[]])

getFinalsStates :: NfaDfaStatesTable -> [Int] -> [Int]
getFinalsStates table finals = map (\x -> snd $ fromJust $ find (\(k, v) -> elem x k) (Map.toList table)) finals

convertNfa2Dfa :: Fsm -> Fsm
convertNfa2Dfa (alphabet, edges, init, finals) = let dStates = eClosure edges [init]
                                                     (table, dfa) = handleDfaState edges (Map.singleton dStates 0) [0] [] alphabet [[]]
                                                 in (alphabet, dfa, 0, getFinalsStates table finals)

convertNfa2Dfa' :: Fsm -> NfaDfaStatesTable
convertNfa2Dfa' (alphabet, edges, init, finals) = let dStates = eClosure edges [init]
                                                      (table, dfa) = handleDfaState edges (Map.singleton dStates 0) [0] [] alphabet [[]]
                                                  in table

testFsm :: [[Edge]]
testFsm = [[(Empty,1),(Empty,7)],[(Empty,2),(Empty,4)],[(Char 'a',3)],[(Empty,6)],[(Char 'b',5)],[(Empty,6)],[(Empty,1),(Empty,7)],[(Char 'a',8)],[(Char 'b',9)],[(Char 'b',10)],[]]

testFsm2 :: [[Edge]]
testFsm2 = [
    [(Empty,1),(Empty,3)],
    [(Char 'a',2)],
    [(Char 'a',2)],
    [(Char 'b',4)],
    [(Char 'b',4)]
 ]