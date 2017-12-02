module EvenlyDivisible where

import Data.Either
import Text.Printf
import Data.List.Split (splitOneOf, splitOn)
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

testCase = ("5 9 2 8\n9 4 7 3\n3 8 6 5", 9)

parseRow :: String -> [Int]
parseRow row = map read $ words row :: [Int]

parseSheet :: String -> [[Int]]
parseSheet = (map parseRow) . (splitOn "\n")

evenlyDivides :: Int -> Int -> Int
evenlyDivides a b
    | mod == 0  = div
    | otherwise = 0
    where (div, mod) = (max a b) `divMod` (min a b)

combinations :: [Int] -> [(Int, Int)]
combinations xs = [(i, j) | i <- xs, j <- xs, i < j]

rowSum :: [Int] -> Int
rowSum = sum . (map (\(a, b) -> evenlyDivides a b)) . combinations

sheetSum :: [[Int]] -> Int
sheetSum = sum . (map rowSum)

test :: (String, Int) -> Either String String
test (input, expected) = if (sheetSum . parseSheet) input == expected
    then Right "Test case passed!"
    else Left $ printf "Got %s, expected %s" (show input) (show expected)

main :: IO ()
main = do
       if isLeft (test testCase)
       then print $ test testCase
       else do input <- readFile "../input"
               print $ (sheetSum . parseSheet . strip) input
