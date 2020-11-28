module Cyk where

import Data.List
import Data.Maybe

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

extractElem :: Eq a => (a -> Bool) -> [a] -> (Maybe a, [a])
extractElem f l = extractElem' f l []
                            where extractElem' f' (x:xs) acc = if f' x 
                                                               then (Just x, acc ++ xs)
                                                               else extractElem' f' xs (acc ++ [x])
                                  extractElem' _ [] acc = (Nothing, acc)

data Terminal = T Char | E
    deriving (Show, Eq, Ord)

newtype NonTerminal = NT String
    deriving (Show, Eq, Ord)

type Grammar = (NonTerminal, [(NonTerminal, [Either Terminal NonTerminal])])

parseGrammarString :: [String] -> String -> [[Either Terminal NonTerminal]] -> [[Either Terminal NonTerminal]]
parseGrammarString _ [] acc     = acc
parseGrammarString nts str@(x:xs) acc =     if x == '|'
                                            then parseGrammarString nts xs ([]:acc)
                                            else let (newElem, offset) = case find (`isPrefixOf` str) nts of
                                                                            Nothing -> if x == '$' then (Left E, 1) else (Left  $ T x, 1)
                                                                            Just nt -> (Right $ NT nt, length nt)
                                                 in parseGrammarString nts (drop offset str) (if null acc then [[newElem]] else (head acc ++ [newElem]): tail acc)

parseGrammar :: [String] -> Grammar
parseGrammar strs = let woSpaces = map (filter (/= ' ')) strs
                        idxs = map (fromJust . elemIndex '-') woSpaces
                        tuples = map (\(s, idx) -> splitAt idx s) (zip woSpaces idxs)
                        nonTerminals = map fst tuples
                        grammarStrings = map (tail . tail . snd) tuples
                        parsed = concatMap (\(nt, s) -> map (\y -> (NT nt, y)) $ parseGrammarString nonTerminals s []) (zip nonTerminals grammarStrings)
                    in (fst $ head parsed, parsed)

toEFreeGrammar :: Grammar -> Grammar
toEFreeGrammar (start,g) = case extractElem (\(_, grStr) -> if grStr == [Left E] then True else False) g of
                    (Nothing, _) -> (start,rmdups g)
                    (Just (nt, _), g') -> toEFreeGrammar (start,(g' ++ foldr (\(nt', grStr') a -> if elem (Right nt) grStr' then a ++ [(nt', filter (/= Right nt) grStr')] else a) [] g'))

removeSingleProductions :: Grammar -> Grammar
removeSingleProductions (start,g) = case extractSingleProduction g of
                                        (Nothing, _) -> (start, g)
                                        (Just (ntToRemove, ntReplacement), g')
                                            -> removeSingleProductions $ (start, map (\(nt, grStr) -> 
                                                                                (if nt == ntToRemove then ntReplacement else nt, map (\x ->
                                                                                    if x == Right ntToRemove then Right ntReplacement else x
                                                                                ) grStr)
                                                                                ) g')
                            where extractSingleProduction g' = extractSingleProduction' g' []
                                    where extractSingleProduction' (x@(nt, grStr):xs) acc = case grStr of
                                                                                            [Right nt'] -> (Just (nt, nt'), acc ++ xs)
                                                                                            _ -> extractSingleProduction' xs (acc ++ [x])
                                          extractSingleProduction' [] acc = (Nothing, acc)

type CnfGrammar = (NonTerminal, [(NonTerminal, Either Char (NonTerminal, NonTerminal))])

findNt :: CnfGrammar -> Either Char (NonTerminal, NonTerminal) -> Maybe NonTerminal
findNt (_,g) x = case find (\(NT nt, y) -> if x == y && isPrefixOf "_NT" nt then True else False) g of
                Nothing -> Nothing
                Just (nt, _) -> Just nt

handleNonTerminal :: (NonTerminal, [Either Terminal NonTerminal]) -> (CnfGrammar, Int) -> (CnfGrammar, Int)
handleNonTerminal (nt, arr) ((start,acc), counter) = case arr of
                                                        [] -> error "Empty grammar string!"
                                                        [Right _] -> error "Unexpected sinle non terminal!"
                                                        [Left E] -> error "Grammar must be e-free!"
                                                        [Left (T ch)] -> ((start, acc ++ [(nt, Left ch)]), counter)
                                                        [Right nt1, Right nt2] -> ((start, acc ++ [(nt, Right (nt1, nt2))]), counter)
                                                        (x:x':xs) -> case x of
                                                                        Left E -> error "Grammar must be e-free!"
                                                                        Left (T y) -> case findNt (start,acc) (Left y) of
                                                                                        Nothing -> let newNt = NT $ "_NT" ++ show counter
                                                                                                   in handleNonTerminal (nt, (Right newNt):x':xs) ((start, acc ++ [(newNt, Left y)]), counter + 1)
                                                                                        Just nt1 -> handleNonTerminal (nt, (Right nt1):x':xs) ((start,acc), counter)
                                                                        Right nt1 -> case x' of
                                                                                        Left E    -> error "Grammar must be e-free!"
                                                                                        Right nt2 -> case findNt (start,acc) (Right (nt1, nt2)) of
                                                                                                        Nothing -> let newNt = NT $ "_NT" ++ show counter
                                                                                                                   in handleNonTerminal (nt, (Right newNt):xs) ((start, acc ++ [(newNt, Right (nt1, nt2))]), counter + 1)
                                                                                                        Just nt3 -> handleNonTerminal (nt, (Right nt3):x':xs) ((start,acc), counter)
                                                                                        Left (T y)-> case findNt (start,acc) (Left y) of
                                                                                                        Nothing -> let newNt = NT $ "_NT" ++ show counter
                                                                                                                   in handleNonTerminal (nt, Right nt1:Right newNt:xs) ((start, acc ++ [(newNt, Left y)]), counter + 1)
                                                                                                        Just nt2 -> handleNonTerminal (nt, Right nt1:Right nt2:xs) ((start,acc), counter)

convertToCnf :: Grammar -> CnfGrammar
convertToCnf (start,g) = handle (snd $ removeSingleProductions $ toEFreeGrammar (start,g)) ((start,[]), 0)
                    where handle (x:xs) acc = handle xs (handleNonTerminal x acc)
                          handle [] ((_, cnf), _) = (start,cnf)

getCYKcell :: CnfGrammar -> String -> Int -> Int -> [NonTerminal]
getCYKcell (start,g) str i j
    | i == j = let ch = str !! i
               in foldr (\(nt, val) b -> if val == Left ch then b ++ [nt] else b) [] g
    | otherwise = let t1 = getCYKcell (start,g) str i i
                      t2 = getCYKcell (start,g) str (i+1) j
                      t3 = getCYKcell (start,g) str i (j-1)
                      t4 = getCYKcell (start,g) str j j
                      f (nt1,nt2) = foldr (\(nt, val) b -> if val == Right (nt1, nt2) then b ++ [nt] else b) [] g
                  in rmdups . concat . filter (/= []) . map f $ ([(x,y) | x <- t1, y <- t2] ++ [(x,y) | x <- t3, y <- t4])

cykCheckString :: [String] -> String -> Bool
cykCheckString g str = let (start,cnf) = (convertToCnf $ parseGrammar g)
                       in elem start $ getCYKcell (start,cnf) str 0 (length str - 1)
