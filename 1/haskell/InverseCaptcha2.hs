module InverseCaptcha2 where

import Data.Char
import Data.Either
import Text.Printf
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

testCases = [
              ("1212", 6),
              ("1221", 0),
              ("123425", 4),
              ("123123", 12),
              ("12131415", 4)
            ]

transformInput :: String -> [Int]
transformInput = (map digitToInt) . strip

check :: Int -> Int -> Int
check a b = if a == b
            then a + b
            else 0

calculateSum :: [Int] -> Int
calculateSum xs = sum $ zipWith (check) (take mid xs) (drop mid xs)
                  where mid = (length xs) `div` 2

test :: [(String, Int)] -> [Either String String]
test = map checkCase
       where checkCase (input, expect) = if ((calculateSum . transformInput) input == expect)
             then Right $ printf "Test case %s passed!" (show input)
             else Left $ printf "Expected %s for %s, got %d." (show expect) (show input) ((calculateSum . transformInput) input)

main :: IO ()
main = do
      if length (lefts (test testCases)) > 0
      then print $ lefts (test testCases)
      else do input <- readFile "../input"
              print $ (calculateSum . transformInput) input
