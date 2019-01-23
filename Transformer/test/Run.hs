import Examples
import Common.Transformer
import Common.MonadRaise
import Raise.Raise
import Raise.RaiseR
import Raise.RaiseCC

import Control.Monad.Trans.Writer
import Control.Monad.State
import Control.Monad.Cont

testUnluckyNumber :: Int -> IO ()
testUnluckyNumber n = do
    putStrLn "---Unlucky Number"; input n; output
    print (unluckyNumber n :: Raise (Either String) (String,Int))
    print (unluckyNumber n :: RaiseR (Either String) (String,Int))
    print (unluckyNumber n :: RaiseCC (Either String) (String,Int))

testRemoveLongWords :: [String] -> IO ()
testRemoveLongWords list = do
    putStrLn $ "\n---Removing of Long Words"; input list; output
    print (removeLongWords list :: Raise [] String)
    print (removeLongWords list :: RaiseR [] String)
    print (removeLongWords list :: RaiseCC [] String)

testCountdown :: Int -> IO ()
testCountdown n = do
    putStrLn $ "\n---Countdown"; input n; output
    observe (countdown n :: Raise IO ())
    observe (countdown n :: RaiseR IO ())
    observe (countdown n :: RaiseCC IO ())

testParityCheck :: [Int] -> IO ()
testParityCheck list = do
    putStrLn $ "\n---Parity Check"; input list; output
    print (parityCheck list :: Raise [] String)
    print (parityCheck list :: RaiseR [] String)
    print (parityCheck list :: RaiseCC [] String)

testSeparate :: String -> IO ()
testSeparate str = do
    putStrLn "\n---Numbers Separately, Characters Separately, Spaces to Hell!"; input str; output
    print $ observe (separate str :: Raise (WriterT (String,String) Maybe) ())   
    print $ observe (separate str :: RaiseR (WriterT (String,String) Maybe) ())
    print $ observe (separate str :: RaiseCC (WriterT (String,String) Maybe) ())

testFailState :: Int -> IO ()
testFailState n = do
    putStrLn "\n---Fail State"; input n; output;
    print $ runState (observe (failState () :: Raise (State Int) Int)) n
    print $ runState (observe (failState () :: RaiseR (State Int) Int)) n
    print $ runState (observe (failState () :: RaiseCC (State Int) Int)) n       

testApplicative :: [Int -> Int] -> [Int] -> IO ()
testApplicative fs xs = do
    putStrLn "\n---Applicative"; input xs; output;
    print $ observe (applicative fs xs :: Raise [] Int)
    print $ observe (applicative fs xs :: RaiseR [] Int)
    print $ observe (applicative fs xs :: RaiseCC [] Int)

testMapping :: (Int -> Int) -> [Int] -> IO ()
testMapping f xs = do
    putStrLn "\n---fmapping"; input xs; output;
    print $ observe (fmapping f xs :: Raise [] Int)
    print $ observe (fmapping f xs :: RaiseR [] Int)
    print $ observe (fmapping f xs :: RaiseCC [] Int)

testMultiplicationTable :: Int -> IO ()
testMultiplicationTable n = do
    putStrLn "\n---Multiplication Table"; input n; output;
    observe (multiplicationTable n :: Raise IO ())
    observe (multiplicationTable n :: RaiseR IO ())
    observe (multiplicationTable n :: RaiseCC IO ())

main :: IO ()
main = do
    putStrLn "* * * E X A M P L E S * * *"
    testUnluckyNumber 13
    testRemoveLongWords exampleStringList
    testCountdown 10
    testParityCheck exampleIntList
    testSeparate exampleString 
    testSeparate exampleString'
    testFailState 41
    testApplicative exampleOperationsList exampleIntList'
    testMapping (^2) exampleIntList'
    testMultiplicationTable 10

input :: (Show a) => a -> IO ()
input a = putStrLn $ "Input: " ++ show a

output :: IO ()
output = putStrLn "Output:"