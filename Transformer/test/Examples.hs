module Examples where

import Common.Transformer
import Common.MonadRaise

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Char (isDigit,ord)

unluckyNumber number = do {
    if number == 13 || number == 666
      then promote $ Left "This is a very bad number!"
      else return ("Good number", number)
}

removeLongWords list = do {
    x <- promote list;
    if length x >= 5
        then raise ""
        else return x 
}

countdown number = do {
    promote $ putStr $ show number ++ " ";
    when (number > 0) (countdown (number - 1));
    when (number == 0) (promote $ print "B-O-O-O-M !!!")
}

parityCheck numbers = do {
    x <- promote numbers;
    if x `mod` 2 == 0
      then return "Even"
      else return "Odd"
}

separate s = helper True s 
  where 
    helper flag [] = do {
        if flag
        then raise ""
        else return ()
    }
    helper flag (c:str) = do {
        if isDigit c
        then do {promote $ tell ([c],""); helper False str}
        else if c == ' ' 
               then do {return (); helper flag str}
               else do {promote $ tell ("",[c]); helper False str}   
    }

failState _ = do {
    s <- promote get;
    if (s == 40) 
        then raise "No"
        else promote $ put 40;
    return (s+1)
}

multiplicationTable n = do {
    x <- return [1..n];
    z <- return $ fmap (,) x;
    w <- return $ z <*> x;
    f w;    
    return ()
} where
    f [] = promote $ putStrLn ""
    f ((x,y):xs) = do {
        when (x + y <= n + 1) (promote $ putStr $ (show $ x * y) ++ " ");
        when (x + y == n + 1) (promote $ putStrLn ""); 
        f xs
    }
    
fmapping f xs = 
    f 
    <$>
    promote xs

applicative fs xs = 
    promote fs 
    <*> 
    promote xs

exampleStringList :: [String]
exampleStringList = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]

exampleIntList :: [Int]
exampleIntList = [1, 5, 9, 0, 31, 33, 99, 17, 4, 10, 101]

exampleIntList' :: [Int]
exampleIntList' = [1..5]

exampleOperationsList :: [Int -> Int]
exampleOperationsList = [(+1),(+2),(^2)] 

exampleString :: String
exampleString = "I Has 3 Apples And 2 Pears. In Total: 3 + 2 = 5. It's Cool!"

exampleString' :: String
exampleString' = "     "