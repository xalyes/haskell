module LrAnalyzer where

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List
import Data.Maybe
import Data.Either
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

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

type Item = (NonTerminal, Int, [Either Terminal NonTerminal])

instance {-# OVERLAPS #-} Show Item where
  show (NT nt, dot, gstr) = nt ++ "->" ++ showGstr dot gstr "" ++ ""
                                where showGstr dot [] acc = if dot == 0 then acc ++ "." else acc
                                      showGstr dot (x:xs) acc = if dot == 0 then showGstr (-1) xs (acc ++ "." ++ showX x) else showGstr (dot - 1) xs (acc ++ showX x)
                                        where showX (Left (T c)) = [c]
                                              showX (Left E) = "_"
                                              showX (Right (NT nt)) = nt

instance {-# OVERLAPS #-} Show [Item] where
    show items = "{ " ++ (foldl (\b a -> b ++ (show a) ++ ", ") "" items) ++ "}\n"

parseGrammarString :: [String] -> String -> [[Either Terminal NonTerminal]] -> [Terminal] -> ([[Either Terminal NonTerminal]], [Terminal])
parseGrammarString _ [] acc alphabet    = (acc, alphabet)
parseGrammarString nts str@(x:xs) acc alph =  if x == '|'
                                              then parseGrammarString nts xs ([]:acc) alph
                                              else let ((newElem, offset), alph') = case find (`isPrefixOf` str) nts of
                                                                                        Nothing -> if x == '$' then ((Left E, 1), alph ++ [E]) else ((Left  $ T x, 1), alph ++ [T x])
                                                                                        Just nt -> ((Right $ NT nt, length nt), alph)
                                                   in parseGrammarString nts (drop offset str) (if null acc then [[newElem]] else (head acc ++ [newElem]): tail acc) alph'

parseGrammar :: [String] -> (Grammar, [Either Terminal NonTerminal])
parseGrammar strs = let woSpaces = map (filter (/= ' ')) strs
                        idxs = map (fromJust . elemIndex '-') woSpaces
                        tuples = map (\(s, idx) -> splitAt idx s) (zip woSpaces idxs)
                        nonTerminals = map fst tuples
                        grammarStrings = map (tail . tail . snd) tuples
                        (parsed', terminals) = foldr (\(p, t) (ps, ts) -> (p:ps, t:ts)) ([],[])
                                                $ map (\(nt, s) -> let (grStr, terminals') = parseGrammarString nonTerminals s [] [] in (map (NT nt,) grStr, terminals')) (zip nonTerminals grammarStrings)
                        parsed = concat parsed'
                    in ((fst $ head parsed, parsed), map Left (rmdups $ concat terminals) ++ map (Right . NT) nonTerminals)

parseGrammar' strs = let ((start, g), alph) = parseGrammar strs 
                     in ((NT "S'", [(NT "S'", [Right start])] ++ g), alph)

closure :: Grammar -> [Item] -> [Item]
closure g items = let process = foldr (\(nt, dot, str) (acc, processedNts) ->
                                                    let currentSym = str !! dot
                                                        currentNt = fromRight (error "illformed program") currentSym
                                                    in if dot /= length str && (isRight $ currentSym) && (not $ elem currentNt processedNts)
                                                       then (acc ++ processNonTerminal currentNt, currentNt:processedNts)
                                                       else (acc, processedNts))
                      firstResult = process ([],[]) items
                      secondResult = process firstResult (fst firstResult)
                      f first second = if first == second then second else f second (process second (fst second))
                  in items ++ (fst $ f firstResult secondResult)
                  where processNonTerminal nt = foldr (\(nt1, str) acc -> if nt1 == nt then acc ++ [(nt1, 0, str)] else acc) [] (snd g)

goto :: Grammar -> [Item] -> Either Terminal NonTerminal -> [Item]
goto g items x = closure g $ map (\(nt, dot, str) -> (nt, dot + 1, str)) $ filter (\(_, dot, str) -> dot /= length str && str !! dot == x) items

calculateItems :: [String] -> [[Item]]
calculateItems input = let
                            ((start, g), alph) = parseGrammar input
                            g' = (NT "S'", [(NT "S'", [Right start])] ++ g)
                            initialSet = [closure g' [(NT "S'", 0, [Right start])]]
                            process = foldr (\items acc' ->
                                        foldr (\x acc -> let newI = goto g' items x
                                                         in if null newI || elem newI acc then acc else acc ++ [newI]) acc' alph)
                            firstResult = process initialSet initialSet
                            secondResult = process firstResult firstResult
                            f first second = if first == second then second else f second (process second second)
                       in f firstResult secondResult

follow' :: Grammar -> (NonTerminal, [Either Terminal NonTerminal]) -> NonTerminal -> [Terminal]
follow' g (ntOrig, gstr) nt = case elemIndex (Right nt) gstr of
                                    Nothing -> []
                                    Just idx -> if length gstr - idx == 1
                                                then if ntOrig /= nt then follow g ntOrig else []
                                                else case gstr !! (idx + 1) of
                                                        Left t -> t:(follow' g (ntOrig, (drop (idx + 1) gstr)) nt)
                                                        Right nt' -> if nt /= nt' then follow g nt' else follow' g (ntOrig, (drop (idx + 1) gstr)) nt

follow :: Grammar -> NonTerminal -> [Terminal]
follow g@(start, g') nt = rmdups $ foldr (\(st, a) b -> b ++ follow' g (st, (a)) nt) [] g'

data Action = S Int | R (NonTerminal, [Either Terminal NonTerminal]) | Accept
    deriving (Eq, Show)

type ActionTable = Map (Int,Terminal) Action
type GotoTable = Map (Int, NonTerminal) Int

instance {-# OVERLAPS #-} Show ActionTable where
  show m = (foldl (\b a -> b ++ (show a) ++ "\n") "" (Map.toList m))

checkedMapInsert :: (Ord k, Show k, Show a, Eq a) => k -> a -> Map k a -> Map k a
checkedMapInsert k v m = if Map.member k m && (m Map.! k) /= v
                         then error ("Failed to insert key " ++ show k ++ " (value: " ++ show v ++ ") in map - already exists! Current Map:\n")
                         else Map.insert k v m

processItem :: [[Item]] -> Grammar -> Int -> [Item] -> Item -> State ActionTable ()
processItem lrSet g n i (NT "S'", 1, [Right (NT _)]) = do { m <- get; put $ checkedMapInsert (n, E) Accept m }
processItem lrSet g n i (nt, dot, str) = if dot == length str
                                         then do
                                            let follows = follow g nt ++ [E]
                                            mapM_ (\a -> do {m <- get; put $ checkedMapInsert (n, a) (R (nt, str)) m }) follows
                                         else if isLeft $ str !! dot
                                              then do
                                                    let (Left term) = str !! dot
                                                    let item = goto g i (Left term)
                                                    m <- get
                                                    case elemIndex item lrSet of
                                                        Nothing -> return ()
                                                        Just idx -> put $ checkedMapInsert (n, (fromLeft (error "ill-formed") (str !! dot))) (S idx) m
                                              else do return ()

buildActionTable' :: Grammar -> [[Item]] -> [[Item]] -> Int -> ActionTable -> ActionTable
buildActionTable' g lrSet [] n t = t
buildActionTable' g lrSet (i:items) n t = buildActionTable' g lrSet items (n+1) (execState (forM_ i (processItem lrSet g n i)) t)

buildActionTable g items = buildActionTable' g items items 0 Map.empty

processNonTerminal :: [[Item]] -> Grammar -> Int -> [Item] -> NonTerminal -> State GotoTable ()
processNonTerminal lrSet g n i nt = let ij = goto g i (Right nt)
                                        maybeIdx = elemIndex ij lrSet
                                    in case maybeIdx of 
                                        Just idx -> do { m <- get; put $ Map.insert (n, nt) (idx) m }
                                        Nothing -> do {return ()}

buildGotoTable' :: Grammar -> [[Item]] -> [[Item]] -> [NonTerminal] -> Int ->  GotoTable -> GotoTable
buildGotoTable' g lrSet [] nts n t = t
buildGotoTable' g lrSet (i:items) nts n t = buildGotoTable' g lrSet items nts (n+1) (execState (forM_ nts (processNonTerminal lrSet g n i)) t)

buildGotoTable g items = let nts = foldr (\a b -> fst a:b) [] (snd g)
                         in buildGotoTable' g items items (nts \\ [NT "S'"]) 0 Map.empty

buildSlrTable strs = let g = fst $ parseGrammar' strs
                         items = calculateItems strs
                     in (buildActionTable g items, buildGotoTable g items)
