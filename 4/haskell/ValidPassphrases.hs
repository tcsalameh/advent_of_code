module ValidPassphrases where

import Text.Printf
import Data.Either (lefts)
import qualified Data.Set as Set
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

testCases = [
        ("aa bb cc dd ee", True),
        ("aa bb cc dd aa", False),
        ("aa bb cc dd aaa", True)
    ]

isValid :: String -> Bool
isValid s = Set.size wordSet == length wordList
        where wordSet  = Set.fromList wordList
              wordList = words s

test :: [(String, Bool)] -> [Either String String]
test = map check
    where check (input, expected) = if isValid input == expected
            then Right "Test case passed!"
            else Left $ printf "Expected %s for %s, got %s" (show expected) (show input) (show $ isValid input)

main :: IO ()
main = do
       if length (lefts (test testCases)) > 0
       then print $ lefts $ test testCases
       else do print "Test cases passed"
               putStrLn "Input file path: "
               inputFile <- readLn
               input <- readFile inputFile
               print $ length $ filter isValid $ lines $ strip input
