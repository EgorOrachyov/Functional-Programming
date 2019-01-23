import System.IO
import Data.List
import Data.Monoid

{-

main :: IO ()
main = do
    putStrLn "What is your name?"
    putStr   "Name: "
    hFlush stdout
    s <- getLine 
    if (s /= "") then putStr "" else main
    putStrLn $ "Hi, " ++ s ++ "!"
    hFlush stdout

-}

{-

findSubstr :: String -> String -> Maybe Int
findSubstr xs ys = findIndex (isPrefixOf xs) (tails ys)
   
main :: IO ()
main = do 
         putStr "Substring: "
         sub <- getLine
         if (sub == "") 
         then putStrLn "Canceled"
         else do
            content <- getDirectoryContents "."
            (mapM_ (\x -> case findSubstr sub x of 
                                    Just _ -> putStrLn ("Removing file: " ++ x) >> removeFile x
                                    Nothing -> return ()) 
                           content)   

-}

{-

data Arrow a b = Arrow { get :: (a -> b) }

a = Arrow (\x -> 4)
b = Arrow (\x -> "w")

main :: IO ()
main = print ( (get b) ( (get a) "c") ) 

-}

{-

data Info = Info { getItems :: [String], getTotal :: Integer }
type Shopping = Writer Info ()

instance Monoid Info where
    mempty = Info { getItems = [] , getTotal = 0 }
    mappend a b = Info { getItems = l1 ++ l2, getTotal = s1 + s2 } where
        l1 = getItems a
        l2 = getItems b
        s1 = getTotal a
        s2 = getTotal b

purchase :: String -> Integer -> Shopping
purchase item cost = do
    tell (Info [item] cost)
    return ()

total :: Shopping -> Integer
total = getTotal . snd . runWriter

items :: Shopping -> [String]
items = getItems . snd. runWriter

-}

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0",1)
divideList' (x:xs) = ("<-" ++ (show x) ++ "/" ++ log, x / a) 
    where
        (log, a) = divideList' xs
   

main :: IO ()
main = print (divideList' [3,4,5])        

