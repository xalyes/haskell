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

data Token = Mul | Or | Ch Char | LeftParenthese | RightParenthese | Terminator
    deriving (Show, Eq, Ord)

{-extractWord :: String -> String -> (Token, String)
extractWord [] acc = (Str acc, [])
extractWord (x:xs) acc | x /= '(' && x/= ')' && x/= '*' && x/= '|' = extractWord xs (acc ++ [x])
                       | otherwise = (Str acc, (x:xs))
                       -}

scanInternal :: String -> [Token] -> [Token]
scanInternal [] tokens         = tokens
scanInternal (x:xs) tokens
                   | x == '('  = tokens ++ [LeftParenthese] ++ scanInternal xs []
                   | x == ')'  = tokens ++ [RightParenthese] ++ scanInternal xs []
                   | x == '*'  = tokens ++ [Mul] ++ scanInternal xs []
                   | x == '|'  = tokens ++ [Or] ++ scanInternal xs []
                   | otherwise = tokens ++ [Ch x] ++ scanInternal xs []

scan str = scanInternal str []

data Annotation = Annotation { nullable     :: Bool
                             , firstpos     :: [Int]
                             , lastpos      :: [Int]
                             , followpos    :: [Int]
                             } deriving (Show, Eq)

data Factor = FactorExpr RegularExpression | FactorChar (Maybe Char) Annotation
    deriving (Show, Eq)

data Term = TermMul Factor Annotation | Term Factor
    deriving (Show, Eq)

data Or = SingleOr Term | OrTerms Term Or Annotation
    deriving (Show, Eq)

data RegularExpression = SingleRegex Or | DoubleRegex Or RegularExpression Annotation
    deriving (Show, Eq)

annotationFactor :: Factor -> Annotation
annotationFactor (FactorChar _ annotation) = annotation
annotationFactor (FactorExpr expr) = annotationExpr expr

annotationTerm :: Term -> Annotation
annotationTerm (TermMul _ annotation) = annotation
annotationTerm (Term factor) = annotationFactor factor

annotationOr :: Or -> Annotation
annotationOr (OrTerms term or annotation) = annotation
annotationOr (SingleOr term) = annotationTerm term

annotationExpr :: RegularExpression -> Annotation
annotationExpr (SingleRegex or) = annotationOr or
annotationExpr (DoubleRegex or expr annotation) = annotation

addPositionsToFollowposFactor :: Factor -> [Int] -> [Int] -> Factor 
addPositionsToFollowposFactor (FactorChar ch annotation) positions filterPos = let id = head $ firstpos annotation
                                                                               in if (elem id filterPos) then FactorChar ch (annotation {
                                                                                      followpos = (followpos annotation) ++ positions
                                                                                }) else (FactorChar ch annotation)
addPositionsToFollowposFactor (FactorExpr expr) positions filterPos = FactorExpr $ addPositionsToFollowposExpr expr positions filterPos

addPositionsToFollowposTerm :: Term -> [Int] -> [Int] -> Term
addPositionsToFollowposTerm (Term factor) positions filterPos = Term $ addPositionsToFollowposFactor factor positions filterPos
addPositionsToFollowposTerm (TermMul factor annotation) positions filterPos = TermMul (addPositionsToFollowposFactor factor positions filterPos) annotation

addPositionsToFollowposOr :: Or -> [Int] -> [Int] -> Or
addPositionsToFollowposOr (SingleOr term) positions filterPos = SingleOr $ addPositionsToFollowposTerm term positions filterPos
addPositionsToFollowposOr (OrTerms term or annotation) positions filterPos = OrTerms (addPositionsToFollowposTerm term positions filterPos) (addPositionsToFollowposOr or positions filterPos) annotation

addPositionsToFollowposExpr :: RegularExpression -> [Int] -> [Int] -> RegularExpression
addPositionsToFollowposExpr (SingleRegex or) positions filterPos = SingleRegex $ addPositionsToFollowposOr or positions filterPos
addPositionsToFollowposExpr (DoubleRegex or expr annotation) positions filterPos = DoubleRegex (addPositionsToFollowposOr or positions filterPos) (addPositionsToFollowposExpr expr positions filterPos) annotation

parseFactor :: State ([Token], Int) Factor
parseFactor = do
    ((token:ts), id) <- get
    case token of
        (Ch ch)        -> do
                            put (ts, id+1)
                            return (FactorChar (Just ch) (Annotation False [id+1] [id+1] []))
        LeftParenthese -> do
                            put (ts, id)
                            expr <- parseExpression
                            ((RightParenthese:rest), id) <- get
                            put (rest, id)
                            return (FactorExpr expr)
        (Terminator)   -> do
                            put (ts, id+1)
                            return (FactorChar Nothing (Annotation False [id+1] [id+1] []))
        _              -> error "Syntax error: failed to parse factor"

parseTerm :: State ([Token], Int) Term
parseTerm = do
    factor <- parseFactor
    (rest, id) <- get
    case rest of
        (Mul:xs) -> do
                        put (xs, (id))
                        let lastposFactor = lastpos . annotationFactor $ factor
                        let firstposFactor = firstpos . annotationFactor $ factor
                        return (TermMul (addPositionsToFollowposFactor factor firstposFactor lastposFactor) (Annotation True firstposFactor firstposFactor []))
        _        -> do
                        put (rest, id)
                        return (Term factor)

parseOr :: State ([Token], Int) Or
parseOr = do
    term <- parseTerm
    (rest, id) <- get
    case rest of 
        (Or:xs) -> do
                        put (xs, id)
                        or <- parseOr
                        let termAnnotation = annotationTerm term
                        let orAnnotation = annotationOr or
                        return (OrTerms term or Annotation {
                            nullable = (nullable termAnnotation || nullable orAnnotation),
                            firstpos = firstpos termAnnotation ++ firstpos orAnnotation,
                            lastpos = lastpos termAnnotation ++ lastpos orAnnotation,
                            followpos = []
                          })
        _       -> do return (SingleOr term)

parseExpression :: State ([Token], Int) RegularExpression
parseExpression = do
    or <- parseOr
    (rest, id) <- get
    case rest of 
        [] -> return (SingleRegex or)
        (RightParenthese:_) -> return (SingleRegex or)
        _ -> do
                expr <- parseExpression
                let exprAnnotation = annotationExpr expr
                let orAnnotation = annotationOr or
                let lastposOr = lastpos . annotationOr $ or
                let firstposExpr = firstpos . annotationExpr $ expr
                return (DoubleRegex (addPositionsToFollowposOr or firstposExpr lastposOr) expr Annotation {
                            nullable = (nullable exprAnnotation && nullable orAnnotation),
                            firstpos = if (nullable orAnnotation)
                                       then firstpos exprAnnotation ++ firstpos orAnnotation
                                       else firstpos orAnnotation,
                            lastpos = if (nullable exprAnnotation)
                                       then lastpos exprAnnotation ++ lastpos orAnnotation
                                       else lastpos exprAnnotation,
                            followpos = []
                          })

parse tokens = let (regex, (rest, id)) = runState parseExpression (tokens, 0)
        in if rest == [] then regex else error "Syntax error: failed to parse regular expression"

parseTokensWithTerminator tokens = parse (tokens ++ [Terminator])

generateNfa :: RegularExpression -> Fsm
generateNfa regex = case regex of
                        DoubleRegex or regex _ -> let (alph1, edges1, init1, finals1) = generateNfa (SingleRegex or)
                                                      (alph2, edges2, init2, finals2) = generateNfa regex
                                                      edges2' = map (\l -> map (\(x, y) -> (x, y + (length $ init edges1))) l) edges2
                                                      finals2' = map (\x -> (x + (length $ init edges1))) finals2
                                                      in (nub $ alph1 ++ alph2,
                                                          init edges1 ++ [(last edges1) ++ (head edges2')] ++ tail edges2',
                                                          init1,
                                                          finals2')
                        SingleRegex or         -> case or of
                                                  SingleOr term   -> case term of
                                                                        Term factor                 -> case factor of
                                                                                                          FactorChar (Just ch) _ -> ([ch], map (\(x, y) -> [(Char x, y)]) (zip [ch] (iterate succ 1)) ++ [[]], 0, [1])
                                                                                                          FactorExpr regex -> generateNfa regex
                                                                        TermMul (FactorExpr regex') _ -> let (alph, edges, initial, finals) = generateNfa regex'
                                                                                                             edges' = map (\l -> map (\(x, y) -> (x, y + 1)) l) edges
                                                                                                             final = length edges' + 1
                                                                                                         in (alph,
                                                                                                            [[(Empty, final), (Empty, 1)]] ++ init edges' ++ [(last edges' ++ [(Empty, 1), (Empty, final)])] ++ [[]],
                                                                                                            initial,
                                                                                                            [final])
                                                                        TermMul (FactorChar (Just ch) _) _   -> let (alph, edges, initial, final) = ([ch], map (\(x, y) -> [(Char x, y)]) (zip [ch] (iterate succ 2)) ++ [[]], 0, 3)
                                                                                                                 in (alph,
                                                                                                                    [[(Empty, final), (Empty, 1)]] ++ init edges ++ [(last edges ++ [(Empty, 1), (Empty, final)])] ++ [[]],
                                                                                                                    initial,
                                                                                                                    [final])
                                                  OrTerms term or _ -> let (alph1, edges1, init1, finals1) = generateNfa (SingleRegex . SingleOr $ term)
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

getFollowposGraphFactor :: Factor -> Map Int (Maybe Char, [Int])
getFollowposGraphFactor (FactorChar ch annotation) = Map.singleton (head . firstpos $ annotation) (ch, followpos annotation)
getFollowposGraphFactor (FactorExpr expr) = getFollowposGraph expr

getFollowposGraphTerm :: Term -> Map Int (Maybe Char, [Int])
getFollowposGraphTerm (TermMul factor _) = getFollowposGraphFactor factor
getFollowposGraphTerm (Term factor) = getFollowposGraphFactor factor

getFollowposGraphOr :: Or -> Map Int (Maybe Char, [Int])
getFollowposGraphOr (OrTerms term or _) = Map.union (getFollowposGraphTerm term) (getFollowposGraphOr or)
getFollowposGraphOr (SingleOr term) = getFollowposGraphTerm term

getFollowposGraph :: RegularExpression -> Map Int (Maybe Char, [Int])
getFollowposGraph (SingleRegex or) = getFollowposGraphOr or
getFollowposGraph (DoubleRegex or expr _) = Map.union (getFollowposGraphOr or) (getFollowposGraph expr)

type Dfa = Map [Int] [(Char, [Int])]

getNotExistsStates :: Dfa -> [[Int]]
getNotExistsStates dfa = (nub . concat . map (\x -> map (\(_, y) -> sort y) x) . Map.elems $ dfa) \\ (Map.keys dfa)

processChar :: Map Int (Maybe Char, [Int]) -> [Int] -> Char -> State Dfa ()
-- processChar followpos dfa _ | traceShow (followpos) False = undefined
processChar followpos s a = do
    let u = nub . concat . map (\x -> let (c, follows) = followpos Map.! x in if c /= (Just a) then [] else follows) $ s
    dtran <- get
    case Map.lookup s dtran of
      Nothing -> do put (Map.insert s [(a, u)] dtran)
      Just x  -> do put (Map.insert s ((a, u):x) dtran)

processStates [] follows alph dfa = dfa
processStates (unmarkedState:_) follows alph dfa = let ((), newDfa) = runState (mapM_ (processChar follows unmarkedState) alph) dfa
                                                   in processStates (getNotExistsStates newDfa) follows alph newDfa

generateDfa :: RegularExpression -> String -> Dfa
generateDfa r@(DoubleRegex or regex annotation) alph = let followposGraph = getFollowposGraph r
                                                           s = sort $ firstpos annotation
                                                       in processStates [s] followposGraph alph (Map.empty)

                                                    