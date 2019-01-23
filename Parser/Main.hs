module Main where

import Parser
import Data.Char
import Prelude hiding (any)

p = char '(' *> many (satisfy (/=')')) <* char ')'
t = (++) <$> token "Egor" <* spaces <*> token "Orachyov"


parseNum = do
    x <- any "0123456789"
    return $ (read x :: Int)

parseNums = do
    x1 <- any "0123456789"
    many $ char ' '
    char '+'
    many $ char ' '
    x2 <- any "0123456789"
    return $ ((read x1) + (read x2) :: Int) 


main = print ( runParser parseNums "17 + 15" )