module Main where

import Parser
import Data.Char
import Prelude hiding (any)
import qualified Control.Applicative as App

p = char '(' *> many (satisfy (/=')')) <* char ')'
t = (++) <$> token "Egor" <* spaces <*> token "Orachyov"


num = do
    x <- any "0123456789"
    return $ (read x :: Int)

plus = do
    char ' '
    char '+'
    char ' '

minus = do
    char ' '
    char '-'
    char ' '    

sum = do
    x <- num
    y <- sums
    return $ x + y

sums = do 
    x <- (plus  >> num) App.<|> pure 0
    y <- (minus >> num) App.<|> pure 0
    r <- sums' App.<|> pure 0
    return $ x - y
        

parseNums = do
    x1 <- any "0123456789"
    many $ char ' '
    char '+'
    many $ char ' '
    x2 <- any "0123456789"
    return $ ((read x1) + (read x2) :: Int) 


main = print ( runParser (sums :: Parser Char Int) "1 + 2" )