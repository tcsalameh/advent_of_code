module InverseCaptcha where

import Data.Char
import Data.Either
import Text.Printf
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

testCases = [("1122", 3),
             ("1111", 4),
             ("1234", 0),
             ("91212129", 9)]

check :: Int -> Int -> Int
check a b = if a == b
            then a
            else 0

calculateSum :: [Int] -> Int
calculateSum (x:xs) = sum $ zipWith (check) (x:xs) (xs ++ [x])
calculateSum _ = 0

transformInput :: String -> [Int]
transformInput = (map digitToInt) . strip

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
