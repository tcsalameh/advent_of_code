module SquareData where

import Text.Printf
import Data.Either (lefts)

testCases = [
    (1, 0),
    (12, 3),
    (23, 2),
    (1024, 31)
  ]

findNearestOddSquare :: Int -> Int
findNearestOddSquare n = find 1 n
            where find z n
                    | z*z > n   = z - 2
                    | otherwise = find (z + 2) n

findPosition :: Int -> (Int, Int)
findPosition n
    | n == z*z        = (i, j)
    | n <= upperRight = (i + (n - (z*z + 1)), j + 1)
    | n <= upperLeft  = (i + z, j + (z*z + z + 2 - n))
    | n <= lowerLeft  = (i + (z*z + 3*(z + 1) - 1 - n), j - z)
    | otherwise       = (i - 1, j + (n - ((z + 2)^2 - 1)))
    where z = findNearestOddSquare n
          upperRight = z*z + z + 1
          upperLeft = z*z + 2*(z + 1)
          lowerLeft = z*z + 3*(z + 1)
          i = - ((z - 1) `div` 2)
          j = - i

mDistance :: (Int, Int) -> Int
mDistance (x, y) = (abs x) + (abs y)

test :: [(Int, Int)] -> [Either String String]
test cases = map check cases
          where check (input, expected) = if (actual input) == expected
                    then Right "Test case passed!"
                    else Left $ printf "Expected %s for %s, got %s" (show expected) (show input) (show $ actual input)
                actual = (mDistance . findPosition)

main :: IO ()
main = do
        if length (lefts (test testCases)) > 0
        then print $ lefts $ test testCases
        else print $ (mDistance . findPosition) 289326

