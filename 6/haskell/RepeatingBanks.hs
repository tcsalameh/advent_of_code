module RepeatingBanks2 where

import Data.List.Split (splitOn)
import Text.Printf
import Data.Either (isLeft)
import qualified Data.Map as M
import qualified Data.Set as S

input = "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4"

testCase = "0\t2\t7\t0"
testCaseExpected = 5

maxBlock :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxBlock (k1, v1) (k2, v2)
    | v1 < v2   = (k2, v2)
    | v1 == v2  = if k1 < k2 then (k1, v1) else (k2, v2)
    | otherwise = (k1, v1)

findHighestBlock :: M.Map Int Int -> (Int,Int)
findHighestBlock = M.foldrWithKey (\k v acc -> maxBlock (k,v) acc) (maxBound::Int,-1)

zero :: M.Map Int Int -> Int -> M.Map Int Int
zero banks bankNum = M.insert bankNum 0 banks

redistribute :: M.Map Int Int -> M.Map Int Int
redistribute banks = go (zero banks (fst highestBlock)) highestBlock
    where go m (_,0) = m
          go m (k,v) = go (inc (next k) m) (next k,v-1)
          next i = if i < (bankSize-1) then (i+1) else 0
          inc idx m = M.insert idx ((m M.! idx) + 1) m
          bankSize = M.size banks
          highestBlock = findHighestBlock banks

countUntilDup :: M.Map Int Int -> (M.Map Int Int, Int)
countUntilDup banks = counter banks set 0
        where counter m s n = if S.member m s
                              then (m, n)
                              else counter (next m) (S.insert m s) (n+1)
              set = S.empty
              next m = redistribute m

test :: M.Map Int Int -> Int -> Either String String
test input exp = if (snd . countUntilDup) input == exp
                    then Right "Test case passed!"
                    else Left $ printf "Expected %s for %s, got %s" (show exp) (show input) (show $ countUntilDup input)

parseInput :: String -> M.Map Int Int
parseInput s = M.fromList $ zip [0..] $ map (\x -> read x::Int) $ splitOn "\t" s

main :: IO ()
main = do
        if isLeft $ test (parseInput testCase) testCaseExpected
        then print $ test (parseInput testCase) testCaseExpected
        else do putStrLn "Count until first repetition: "
                print $ (snd . countUntilDup) $ parseInput input
                putStrLn "Count from first to second repetition: "
                print $ (snd . countUntilDup . fst . countUntilDup) $ parseInput input
