module ValueSums where

import Text.Printf
import Data.Either (lefts)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

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

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (i, j) = [(i-1, j-1),
                    (i-1, j),
                    (i-1, j+1),
                    (i, j-1),
                    (i, j+1),
                    (i+1, j-1),
                    (i+1, j),
                    (i+1, j+1)]

sumNeighbors :: (Int, Int) -> Map.Map (Int, Int) Int -> Int
sumNeighbors coords mapping = foldr (+) 0 $ mapMaybe (\(x,y) -> Map.lookup (x,y) mapping) (neighbors coords)

buildMap :: Int -> Map.Map (Int, Int) Int
buildMap 1 = Map.singleton (0, 0) 1
buildMap n = Map.union prev (Map.singleton coords (sumNeighbors coords prev))
             where coords = findPosition n
                   prev   = buildMap (n-1)

-- very inefficient for large values since
-- we rebuild the map from scratch for all n,
-- but the puzzle input only requires a map
-- of <100 entries so not optimizing for now.
findNearest :: Int -> Int -> Int
findNearest value n
    | value <= maxSoFar = maxSoFar
    | otherwise         = findNearest value (n+1)
    where maxSoFar = maximum $ Map.elems $ buildMap n

main :: IO ()
main = do
        putStrLn "Enter value: "
        value <- getLine
        print $ findNearest (read value::Int) 1

