module RepeatingBanks2 where

import Data.List.Split (splitOn)
import Text.Printf
import Data.Either (isLeft)
import qualified Data.Vector as V
import qualified Data.Set as S

input = "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4"

testCase = "0\t2\t7\t0"
testCaseExpected = 5

maxBlock :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxBlock (k1, v1) (k2, v2)
    | v1 < v2   = (k2, v2)
    | v1 == v2  = if k1 < k2 then (k1, v1) else (k2, v2)
    | otherwise = (k1, v1)

findHighestBlock :: V.Vector Int -> (Int,Int)
findHighestBlock = V.ifoldr (\i v acc -> maxBlock (i,v) acc) (maxBound::Int,-1)

zero :: Int -> V.Vector Int -> V.Vector Int
zero bankNum v = v V.// [(bankNum, 0)]

redistribute :: V.Vector Int -> V.Vector Int
redistribute banks = go (zero (fst highestBlock) banks) highestBlock
    where go v (_,0) = v
          go v (i,x) = go (inc (next i) v) (next i,x-1)
          next i = if i < (banksize-1) then (i+1) else 0
          inc i v = v V.// [(i, (v V.! i) + 1)]
          banksize = length banks
          highestBlock = findHighestBlock banks

countUntilDup :: V.Vector Int -> (V.Vector Int, Int)
countUntilDup banks = counter banks S.empty 0
        where counter v s n = if S.member v s
                              then (v, n)
                              else counter (redistribute v) (S.insert v s) (n+1)

test :: V.Vector Int -> Int -> Either String String
test input exp = if (snd . countUntilDup) input == exp
                    then Right "Test case passed!"
                    else Left $ printf "Expected %s for %s, got %s" (show exp) (show input) (show $ countUntilDup input)

parseInput :: String -> V.Vector Int
parseInput s = V.fromList $ map (\x -> read x::Int) $ splitOn "\t" s

main :: IO ()
main = do
        if isLeft $ test (parseInput testCase) testCaseExpected
        then print $ test (parseInput testCase) testCaseExpected
        else do putStrLn "Count until first repetition: "
                print $ (snd . countUntilDup) $ parseInput input
                putStrLn "Count from first to second repetition: "
                print $ (snd . countUntilDup . fst . countUntilDup) $ parseInput input
