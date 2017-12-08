module Registers where

import Text.Printf
import Text.ParserCombinators.Parsec
import Control.Monad (ap)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (foldl')

-- test case --

testCase = "b inc 5 if a > 1\n\
\a inc 1 if b < 5\n\
\c dec -10 if a >= 1\n\
\c inc -20 if c == 10\n"

-- Data types --

type Register = String
type RegisterMap = Map Register Int
type Action = (Int -> Int)

data Condition a =
    Cond { op :: (a -> a -> Bool)
         , reg :: Register
         , val :: Int }

data Instruction =
    Instr Register Action Int (Condition Int)

type Program = [Instruction]

-- Parser --

file = endBy line eol

eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

line = do
    register <- many letter
    space
    act <- action
    space
    amount <- ap sign $ read <$> (many digit)
    string " if "
    compReg <- many letter
    space
    compare <- comparison
    space
    compVal <- ap sign $ read <$> (many digit)
    return $ Instr register act amount $ Cond { op = compare
                                              , reg = compReg
                                              , val = compVal }

sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)
action = (string "dec" >> return negate) <|> (string "inc" >> return id)
comparison = try (string ">=" >> return (>=))
         <|> try (string "<=" >> return (<=))
         <|> (string ">" >> return (>))
         <|> (string "<" >> return (<))
         <|> (string "!=" >> return (/=))
         <|> (string "==" >> return (==))

parseFile :: String -> Either ParseError Program
parseFile = parse file "(unknown)"

-- Functions --

lookupR :: RegisterMap -> Register -> (Int, RegisterMap)
lookupR m r = if M.member r m
              then (m M.! r, m)
              else (0, M.insert r 0 m)

updateR :: RegisterMap -> Register -> Int -> Action -> RegisterMap
updateR m r v a = M.insert r new intermed
    where (cur, intermed) = lookupR m r
          new = cur + (a v)

evalC :: RegisterMap -> Condition Int -> (Bool, RegisterMap)
evalC m cond = ((op cond) fstVal (val cond), newM)
    where (fstVal, newM) = lookupR m (reg cond)

evalI :: RegisterMap -> Instruction -> RegisterMap
evalI m (Instr r action amnt c) =
    if cond
    then updateR intermed r amnt action
    else intermed
    where (cond, intermed) = evalC m c

maxRegValue :: RegisterMap -> Int
maxRegValue = maximum . M.elems

evalAndCheckMax :: RegisterMap -> Instruction -> Int -> (RegisterMap, Int)
evalAndCheckMax m i oldMax = (newMap, max (maxRegValue newMap) oldMax)
    where newMap = evalI m i

evalP :: Program -> (RegisterMap, Int)
evalP = foldl' (\(m,v) i -> evalAndCheckMax m i v) (M.empty, 0)

maxes :: (RegisterMap, Int) -> (Int, Int)
maxes (m, totalMax) = (maxRegValue m, totalMax)

main :: IO ()
main = do
    putStrLn "Input file: "
    fileName <- readLn
    input <- readFile fileName
    case parseFile input of
        Left error -> print error
        Right program -> do (uncurry (printf "Max current value: %d, Max liftime value: %d\n"))
                                $ (maxes . evalP) program
