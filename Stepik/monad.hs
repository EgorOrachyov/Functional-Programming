module Monad where

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
	fmap f (Entry k v) = Entry k (f v)  

instance Functor (Map k1 k2) where
	fmap _ (Map []) = Map []
	fmap f (Map (x:xs)) = Map ((fmap f x):rest) where
    	Map rest = fmap f (Map xs)

-------------------------------------------------------------

data Log a = Log [String] a

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (\x -> (Log ([msg]) (f x)))

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (a ++ b) r2 where
	Log a r1 = f x
	Log b r2 = g r1

main :: IO ()
main = print ()