module Registers where

import Text.Printf
import Text.ParserCombinators.Parsec

file = endBy line eol

eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

type Register = Char

data Action =
      Inc
    | Dec
    deriving (Eq, Show)

data Condition a =
    Cond { op :: (a -> a -> Bool)
         , reg :: Register
         , val :: Int }

data Instruction =
    Instr Register Action Int (Condition Int)

line = do
    register <- letter
    space
    action <- inc <|> dec
    space
    amount <- read <$> many digit
    string " if "
    compReg <- letter
    space
    compare <- gt <|> lt <|> eq <|> geq <|> lte <|> neq
    space
    compVal <- read <$> many digit
    return $ Instr register (getAction action) amount $ Cond { op = (comparison compare)
                                                             , reg = compReg
                                                             , val = compVal }

inc = string "inc"
dec = string "dec"
gt = string ">"
lt = string "<"
geq = string ">="
lte = string "<="
neq = string "!="
eq = string "=="


getAction :: String -> Action
getAction "dec" = Dec
getAction _ = Inc

comparison :: Ord a => String -> (a -> a -> Bool)
comparison s
    | s == "==" = (==)
    | s == "<=" = (<=)
    | s == ">=" = (>=)
    | s == "<"  = (<)
    | s == ">"  = (>)
    | otherwise = (/=)

