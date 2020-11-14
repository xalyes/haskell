module Regexps where

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
-- getNewState fsm table nfaStates a | traceShow (nfaStates) False = undefined
getNewState fsm table nfaStates a = let newNfaStates = eClosure fsm (move fsm nfaStates a)
                                        newDfaState = (Map.foldr max 0 table) + 1
                                        newTable = Map.insertWith (\newVal oldVal -> oldVal) newNfaStates newDfaState table
                                    in if newNfaStates == [] then (table, -1) else (newTable, newTable Map.! newNfaStates)

addTransition :: [[Edge]] -> Int -> Char -> Int -> [[Edge]]
addTransition fsm t a u = modifyElem t fsm (fsm !! t ++ [(Char a, u)])

-- getNewDTransition nfa a table t dStates dfa | traceShow (t) False = undefined
getNewDTransition nfa a table t dStates dfa = let (newTable, u) = getNewState nfa table (findByValue table t) a
                                                  newDStates = if elem u dStates then dStates else dStates ++ [u]
                                                  newDfa = addTransition dfa t a u
                                              in if u /= -1 then (newTable, newDStates, newDfa) else (table, dStates, dfa)

handleDfaState :: [[Edge]] -> NfaDfaStatesTable -> [Int] -> [Int] -> [Char] -> [[Edge]] -> (NfaDfaStatesTable, [[Edge]])
-- handleDfaState _ table dStates markedStates _ dfa | traceShow (dfa) False = undefined
handleDfaState nfa table dStates markedStates alph dfa | sort dStates == sort markedStates = (table, init dfa)
handleDfaState nfa table dStates markedStates alph dfa | otherwise               = let t = head $ dStates \\ markedStates
                                                                                       (newTable, newDStates, newDfa) = foldl (\(table', dStates', dfa') a -> getNewDTransition nfa a table' t dStates' dfa')
                                                                                          (table, dStates, dfa) alph
                                                                                   in handleDfaState nfa newTable newDStates (t:markedStates) alph (newDfa ++ [[]])

getFinalsStates :: NfaDfaStatesTable -> [Int] -> [Int]
getFinalsStates table finals = let indices = nub . concat . map (\x -> findIndices (\(k, v) -> elem x k) (Map.toList table)) $ finals
                               in map (\x -> snd (Map.toList table !! x)) indices

convertNfa2Dfa :: Fsm -> Fsm
convertNfa2Dfa (alphabet, edges, init, finals) = let dStates = eClosure edges [init]
                                                     (table, dfa) = handleDfaState edges (Map.singleton dStates 0) [0] [] alphabet [[]]
                                                 in (alphabet, dfa, 0, getFinalsStates table finals)

checkString :: Fsm -> String -> Bool
checkString (_, edges, init, finals) str = let s0 = eClosure edges [init]
                                               checkString' finals states []     = if (intersect finals states == []) then False else True
                                               checkString' finals states (x:xs) = let newStates = eClosure edges (move edges states x)
                                                                                   in checkString' finals newStates xs
                                           in checkString' finals s0 str

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

testFsm3 :: [[Edge]]
testFsm3 = [
    [(Char 'a',1),(Char 'a',0),(Char 'b',0)],
    [(Char 'a',2),(Char 'a',1),(Char 'b',1)],
    [(Char 'b',3),(Char 'a',2),(Char 'b',2), (Empty, 0)],
    []
 ]

testFsm4 :: [[Edge]]
testFsm4 = [
    [(Empty,3),(Char 'a',1)],
    [(Empty,0),(Char 'b',2)],
    [(Empty,1),(Char 'b',3)],
    [(Empty,2),(Char 'a',0)]
 ]

testFsm5 :: Fsm
testFsm5 = ("ab", [[(Empty,7),(Empty,1)],[(Empty,2),(Empty,4)],[(Char 'a',3)],[(Empty,6)],[(Char 'b',5)],[(Empty,6)],[(Empty,1),(Empty,7)],[]], 0, [7])

data Token = Mul | Or | Str String | LeftParenthese | RightParenthese
    deriving (Show, Eq, Ord)

extractWord :: String -> String -> (Token, String)
extractWord [] acc = (Str acc, [])
extractWord (x:xs) acc | x /= '(' && x/= ')' && x/= '*' && x/= '|' = extractWord xs (acc ++ [x])
                       | otherwise = (Str acc, (x:xs))

scanInternal :: String -> [Token] -> [Token]
scanInternal [] tokens         = tokens
scanInternal (x:xs) tokens
                   | x == '('  = tokens ++ [LeftParenthese] ++ scanInternal xs []
                   | x == ')'  = tokens ++ [RightParenthese] ++ scanInternal xs []
                   | x == '*'  = tokens ++ [Mul] ++ scanInternal xs []
                   | x == '|'  = tokens ++ [Or] ++ scanInternal xs []
                   | otherwise = let (word, rest) = extractWord xs [x]
                                 in tokens ++ [word] ++ scanInternal rest []

scan str = scanInternal str []

data Factor = FactorExpr RegularExpression | FactorStr String
    deriving (Show, Eq)

data Term = TermMul Factor | Term Factor
    deriving (Show, Eq)

data Or = SingleOr Term | OrTerms Term Or
    deriving (Show, Eq)

data RegularExpression = SingleRegex Or | DoubleRegex Or RegularExpression
    deriving (Show, Eq)

parseFactor :: State [Token] Factor
parseFactor = do
    (token:ts) <- get
    put ts
    case token of
        (Str str)      -> do
                            return (FactorStr str)
        LeftParenthese -> do
                            expr <- parseExpression
                            (RightParenthese:rest) <- get
                            put rest
                            return (FactorExpr expr)
        _              -> error "Syntax error: failed to parse factor"

parseTerm :: State [Token] Term
parseTerm = do
    factor <- parseFactor
    rest <- get
    case rest of
        (Mul:xs) -> do
                        put xs
                        return (TermMul factor)
        _        -> do
                        put rest
                        return (Term factor)

parseOr :: State [Token] Or
parseOr = do
    term <- parseTerm
    rest <- get
    case rest of 
        (Or:xs) -> do
                        put xs
                        or <- parseOr
                        return (OrTerms term or)
        _       -> do return (SingleOr term)

parseExpression :: State [Token] RegularExpression
parseExpression = do
    or <- parseOr
    rest <- get
    case rest of 
        [] -> return (SingleRegex or)
        (RightParenthese:_) -> return (SingleRegex or)
        _ -> do
                expr <- parseExpression
                return (DoubleRegex or expr)

parse tokens = let (regex, rest) = runState parseExpression tokens
        in if rest == [] then regex else error "Syntax error: failed to parse regular expression"

generateNfa :: RegularExpression -> Fsm
generateNfa regex = case regex of
                        DoubleRegex or regex -> let (alph1, edges1, init1, finals1) = generateNfa regex
                                                    (alph2, edges2, init2, finals2) = generateNfa (SingleRegex or)
                                                    edges2' = map (\l -> map (\(x, y) -> (x, y + (length $ init edges1))) l) edges2
                                                    finals2' = map (\x -> (x + (length $ init edges1))) finals2
                                                    in (nub $ alph1 ++ alph2,
                                                        init edges1 ++ [(last edges1) ++ (head edges2')] ++ tail edges2',
                                                        init1,
                                                        finals2')
                        SingleRegex or       -> case or of
                                                  SingleOr term   -> case term of
                                                                        Term factor                 -> case factor of
                                                                                                          FactorStr "ϵ"    -> ("", [[(Empty, 1)], []], 0, [1])
                                                                                                          FactorStr str    -> (nub str, map (\(x, y) -> [(Char x, y)]) (zip str (iterate succ 1)) ++ [[]], 0, [length str])
                                                                                                          FactorExpr regex -> generateNfa regex
                                                                        TermMul (FactorExpr regex') -> let (alph, edges, initial, finals) = generateNfa regex'
                                                                                                           edges' = map (\l -> map (\(x, y) -> (x, y + 1)) l) edges
                                                                                                           final = length edges' + 1
                                                                                                       in (alph,
                                                                                                          [[(Empty, final), (Empty, 1)]] ++ init edges' ++ [(last edges' ++ [(Empty, 1), (Empty, final)])] ++ [[]],
                                                                                                          initial,
                                                                                                          [final])
                                                                        TermMul (FactorStr "ϵ")     -> ("", [[(Empty,1), (Empty,3)], [(Empty, 2)], [(Empty, 1), (Empty, 3)], []], 0, [3])
                                                                        TermMul (FactorStr str)     -> let (alph, edges, initial, final) = (nub str, map (\(x, y) -> [(Char x, y)]) (zip str (iterate succ 2)) ++ [[]], 0, length str + 2)
                                                                                                       in (alph,
                                                                                                          [[(Empty, final), (Empty, 1)]] ++ init edges ++ [(last edges ++ [(Empty, 1), (Empty, final)])] ++ [[]],
                                                                                                          initial,
                                                                                                          [final])
                                                  OrTerms term or -> let (alph1, edges1, init1, finals1) = generateNfa (SingleRegex . SingleOr $ term)
                                                                         (alph2, edges2, init2, finals2) = generateNfa (SingleRegex or)
                                                                         edges1' = map (\l -> map (\(x, y) -> (x, y + 1)) l) edges1
                                                                         edges2' = map (\l -> map (\(x, y) -> (x, y + (length edges1) + 1)) l) edges2
                                                                         finals2' = map (\x -> (x + (length $ init edges1))) finals2
                                                                         newLastStateIdx = 1 + length edges1 + length edges2
                                                                         edges1'' = modifyElem (length edges1' - 1) edges1' ((edges1' !! (length edges1' - 1)) ++ [(Empty, newLastStateIdx)])
                                                                         edges2'' = modifyElem (length edges2' - 1) edges2' ((edges2' !! (length edges2' - 1)) ++ [(Empty, newLastStateIdx)])
                                                                     in (nub $ alph1 ++ alph2,
                                                                        [[(Empty, 1), (Empty, length edges1'' + 1)]] ++ edges1'' ++ edges2'' ++ [[]],
                                                                        0,
                                                                        [newLastStateIdx])

checkMatch :: String -> String -> Bool
checkMatch regex = checkString (convertNfa2Dfa $ generateNfa $ parse (scan regex))
