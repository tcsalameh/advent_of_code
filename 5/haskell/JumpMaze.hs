module JumpMaze where

import Text.Printf
import Data.Map (Map, (!), size, fromList, insert)
import Data.Either (isLeft)

testCase = ([0, 3, 0, 1, -3], 5)

jump :: Map Int Int -> Int -> (Map Int Int, Int)
jump m idx = (insert idx ((m ! idx) + 1) m, idx + (m ! idx))

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

