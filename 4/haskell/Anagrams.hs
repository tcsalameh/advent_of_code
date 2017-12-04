module Anagrams where

import Text.Printf
import Data.Either (lefts)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.List as L

strip = T.unpack . T.strip . T.pack

testCases = [
    ("abcde fghij", True),
    ("abcde xyz ecdab", False),
    ("a ab abc abd abf abj", True),
    ("iiii oiii ooii oooi oooo", True),
    ("oiii ioii iioi iiio", False),
    ("nyot babgr babgr kqtu kqtu kzshonp ylyk psqk", False)
  ]

isValid :: String -> Bool
isValid s = all (uncurry validWords) [(a, b) | a <- xs, b <- xs, a < b]
            && noDupes xs
            where xs = words s

noDupes :: [String] -> Bool
noDupes wordList = Set.size (Set.fromList wordList) == length wordList

validWords :: String -> String -> Bool
validWords a b = not $ sameLetters a b

sameLetters :: String -> String -> Bool
sameLetters a b = L.sort a == L.sort b

test :: [(String, Bool)] -> [Either String String]
test = map check
       where check (input, expected) = if isValid input == expected
                    then Right "Test case passed!"
                    else Left $ printf "Expected %s for %s, got %s" (show expected) (show input) (show (isValid input))

main :: IO ()
main = do
       if length (lefts (test testCases)) > 0
       then print $ lefts $ test testCases
       else do print "Test cases passed"
               putStrLn "Input file path: "
               inputFile <- readLn
               input <- readFile inputFile
               print $ length $ filter isValid $ lines $ input
