module Tower where

import Text.Printf
import Text.ParserCombinators.Parsec
import Data.Map (Map, (!))
import Data.List (minimumBy, sort, sortBy, groupBy)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Graph

file = endBy line eol
child = sepBy1 entry (string ", ")
entry = many1 letter

line = do
    root <- many letter
    space >> char '('
    weight <- read <$> many digit
    char ')'
    optional (string " -> ")
    children <- many child
    return (weight, root, concat children)

eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

parseFile = parse file "(unknown)"

tFst (x, _, _) = x
tMid (_, x, _) = x
tLst (_, _, x) = x

findRoot :: [(Int, String, [String])] -> String
findRoot lst = tMid $ fromV $ (head . topSort) graph
    where (graph, fromV, _) = graphFromEdges lst

buildWeights :: Map String (Int, [String]) -> Map String Int
buildWeights m = M.map go m
    where go (w,l)  = sum (w:rest l)
          rest l = map (\k -> go (m ! k)) l

oddOneOut :: [(String, Int)] -> Maybe (String, (Int, Int))
oddOneOut xs = if (length counts) <= 1
               then Nothing
               else Just $ (fst (oddOne counts), (snd (oddOne counts), (snd (oddOne counts)) - (snd (others counts))))
    where counter = map (head &&& length) . groupBy (\(k,v) (k',v') -> k == k') . sort
          counts = counter (map swap xs)
          oddOne = swap . fst . head . (sortBy (\(k,v) (k',v') -> v `compare` v'))
          others = swap . fst . last . (sortBy (\(k,v) (k',v') -> v `compare` v'))

findCorrectedWeight :: [(Int, String, [String])] -> Int
findCorrectedWeight input = fst (nMap ! (fst oddOne))  - (snd (snd oddOne))
    where nMap = M.fromList $ map (\(w, k, l) -> (k, (w, l))) input
          weights = buildWeights nMap
          tree = M.fromList $ map (\(w, k, l) -> (k, l)) input
          groups = M.elems $ M.map (\v -> map (\i -> (i, weights ! i)) v) tree
          oddOne = minimumBy (\(k, v) (k',v') -> v `compare` v') $ mapMaybe (oddOneOut) groups

main :: IO ()
main = do
    putStrLn "Input file: "
    fileName <- readLn 
    input <- readFile fileName
    case parseFile input of
        Left error   -> do print "failed to parse!"
        Right result -> do printf "Base program: %s\n" (findRoot result)
                           printf "Corrected Weight: %s\n" (show $ findCorrectedWeight result)
