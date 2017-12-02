module SpreadsheetChecksum where

import Data.Either
import Text.Printf
import Control.Monad
import Data.List.Split (splitOneOf, splitOn)
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

testCase = ("5 1 9 5\n7 5 3\n2 4 6 8", 18)

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just $ foldr (\x (low, high) -> ((min low x), (max high x))) (x, x) xs

parseRow :: String -> [Int]
parseRow row = map read $ words row :: [Int]

parseSheet :: String -> [[Int]]
parseSheet = (map parseRow) . (splitOn "\n")

rowCS :: [Int] -> Maybe Int
rowCS row = fmap (\(minN, maxN) -> maxN - minN) mm
            where mm = minMax row

sheetCS :: [[Int]] -> Maybe Int
sheetCS = (foldM (\x acc -> fmap (+x) acc) 0) . (map rowCS)

test :: (String, Int) -> Either String String
test (input, expected) = if (sheetCS . parseSheet) input == Just expected
    then Right "Test case passed!"
    else Left $ printf "Got %s, expected %s" (show input) (show expected)

main :: IO ()
main = do
       if isLeft (test testCase)
       then print $ test testCase
       else do input <- readFile "../input"
               print $ (sheetCS . parseSheet . strip) input
