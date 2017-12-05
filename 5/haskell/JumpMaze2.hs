module Main where

import Text.Printf
import Data.Map (Map, (!), size, fromList, insert)
import Data.Either (isLeft)

-- Interestingly, this results in a stack overflow
-- when run by loading into ghci, but not when run by compiling
-- with ghc and then running the executable.
-- This is likely because ghc has some additional
-- optimizations around unrolling recursion that ghci
-- does not. Still, very slow in either case and there's
-- probably a lot of optimization I could do here.

testCase = ([0, 3, 0, 1, -3], 10)

offsetDiff :: Int -> Int
offsetDiff n
    | n >= 3 = -1
    | otherwise = 1

jump :: Map Int Int -> Int -> (Map Int Int, Int)
jump m idx = (insert idx newOffset m, idx + offset)
             where offset = m ! idx
                   newOffset = offset + (offsetDiff offset)

runJumps :: [Int] -> Int
runJumps instrs = run (m,0) 0
        where run (m,idx) n
                | idx >= size m || idx < 0 = n
                | otherwise = run (jump m idx) (n+1)
              m = fromList $ zip [0..] instrs

test :: ([Int], Int) -> Either String String
test (input, expected)
    | (runJumps input) == expected = Right "Test case passed!"
    | otherwise = Left $ printf "Expected %s for %s, got %s" (show expected) (show input) (show $ runJumps input)

main :: IO ()
main = do
        if isLeft $ test testCase
        then print $ test testCase
        else do putStrLn "Input file: "
                fileName <- readLn
                input <- readFile fileName
                print $ runJumps $ map (\x -> read x::Int) $ lines input

