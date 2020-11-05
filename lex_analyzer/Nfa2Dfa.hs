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
import qualified Data.Map as Map

data Symbol = Empty | Char Char
    deriving (Show, Eq)

type Edge = (Symbol, Int)
type Fsm = ([[Edge]], Int, [Int])

getTransitions :: [Edge] -> Symbol -> [Int]
getTransitions edges sym = map (snd) . filter (\(sym', s) -> sym' == sym) $ edges

eClosure :: [[Edge]] -> [Int] -> [Int]
eClosure fsm states = let tmp = filter (\s -> not (elem s states)) . nub . concat . map (\s -> getTransitions (fsm !! s) Empty) $ states
                      in if tmp == [] then states else eClosure fsm (tmp ++ states)

convertNfa2Dfa :: Fsm -> Fsm
convertNfa2Dfa (edges, init, finals) = undefined
