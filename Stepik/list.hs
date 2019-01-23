import Data.List
import Data.Ord
import Data.Char



oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) = if (odd x) then (x:(oddsOnly xs)) else (oddsOnly xs)


isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = check (reverse l) l where
    check :: Eq a => [a] -> [a] -> Bool
    check [] [] = True
    check (a:as) (b:bs) = ((a == b) && (check as bs))


sum2 :: Num a => [a] -> [a] -> [a]
sum2 (a:as) (b:bs) = (a + b):(sum2 as bs)
sum2 (a:as) _      = a:as
sum2 _      (b:bs) = b:bs
sum2 _      _      = [] 


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a:as) (b:bs) (c:cs) = (a + b + c):(sum3 as bs cs)
sum3 []     b      c      = sum2 b c
sum3 a      []     c      = sum2 a c
sum3 a      b      []     = sum2 a b


groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:[]) = [[x]]
groupElems (prev:curr:xs) = helper prev curr [prev] xs where
    helper :: Eq a => a -> a -> [a] -> [a] -> [[a]]
    helper p c a [] = if (p == c) 
        then [c:a] 
        else [a,[c]]
    helper p c a (x:xs) = if (p == c) 
        then (helper c x (c:a) xs) 
        else a:(helper c x ([c]) xs)

-------------------------------------------------

readDigits :: String -> (String, String)
readDigits l = helper [] l where
    helper :: String -> String -> (String, String)
    helper a [] = (a,[])
    helper a rest@(x:xs) 
        | isDigit x = helper (a ++ [x]) xs
        | otherwise = (a, rest)


filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g [] = []
filterDisj f g (x:xs) 
    | (f x) || (g x) = x:(filterDisj f g xs)
    | otherwise = filterDisj f g xs        


qsort :: Ord a => [a] -> [a]
qsort [] = [] 
qsort (x:xs) = smallerSorted ++ [x] ++ biggerSorted where
    smallerSorted = qsort [a | a <- xs, a <= x] 
    biggerSorted = qsort [a | a <- xs, a > x] 

-------------------------------------------------

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes [] = []
squares'n'cubes (x:xs) = [x * x, x * x * x] ++ (squares'n'cubes xs)


delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 func where
    func :: Ord a => a -> a -> a -> a
    func a b c = max a (max b c) 

fibStream :: [Integer]
fibStream = 0:1:1:2:fibs 1 2 where
    fibs :: Integer -> Integer -> [Integer]
    fibs a b = (a + b):(fibs b (a + b)) 

repeatHelper :: a -> a
repeatHelper x = x        

-------------------------------------------------

data Odd = Odd Integer 
  deriving (Eq, Show)

instance Enum Odd where
    
    succ (Odd v) = Odd (v + 2)
    
    pred (Odd v) = Odd (v - 2)
    
    toEnum e = Odd (toInteger e)

    fromEnum (Odd e) 
        | e > (toInteger (maxBound :: Int)) = error ""
        | e < (toInteger (minBound :: Int)) = error ""
        | otherwise = (fromInteger e) :: Int

    enumFromThenTo a@(Odd x) (Odd y) (Odd z)
        | (x <= y) && (y <= z) = a:(listGen (y - x) y z) where 
            listGen :: Integer -> Integer -> Integer -> [Odd]
            listGen d y z
                | y > z = []
                | otherwise = (Odd y):(listGen d (y + d) z)
    enumFromThenTo a@(Odd x) (Odd y) (Odd z)            
        | (x >= y) && (y >= z) = a:(listGen (y - x) y z) where 
            listGen :: Integer -> Integer -> Integer -> [Odd]
            listGen d y z
                | y < z = []
                | otherwise = (Odd y):(listGen d (y + d) z)        
    enumFromThenTo a@(Odd x) (Odd y) (Odd z)            
        | (x <= z) && (y > z) = [a]
        | (x >= z) && (y < z) = [a]
        | otherwise = [] 

    enumFromTo a@(Odd x) b@(Odd y) 
        | x < y = enumFromThenTo a (succ a) b
        | x > y = []
        | otherwise = [a]

    enumFromThen a@(Odd x) b@(Odd y) = listGen x (y - x) where
        listGen :: Integer -> Integer -> [Odd]
        listGen x d = (Odd x):(listGen (x + d) d) 

    enumFrom a = enumFromThen a (succ a)   

coins = [2, 3, 7]
 
-- change :: (Ord a, Num a) => a -> [[a]]
change :: Integer -> [[Integer]]
change 0 = [[]]
change s = wallets where
    wallets = helper 0 coins [] where
        helper :: Integer -> [Integer] -> [Integer] -> [[Integer]]
        helper _ [] _ = []
        helper v l@(x:xs) h
            | v == s = [h]
            | v > s = [] 
            | otherwise = (helper (v + x) l (x:h)) ++ (helper v (xs++[x]) h) 

-------------------------------------------------

concatList :: [[a]] -> [a]
concatList x = foldr (++) [] x  

lengthList :: [a] -> Int
lengthList x = foldr (\x y -> y + 1) 0 x

sumOdd :: [Integer] -> Integer
sumOdd x = foldr (\x s -> (if (odd x) then s + x else s)) 0 x

meanList :: [Double] -> Double
meanList x =  (\x -> (fst x) / (snd x)) (foldr (\x s -> ((fst s) + x, (snd s) + 1)) (0,0) x)

-- evenOnly :: [a] -> [a]
-- evenOnly x = (\x -> snd x) (foldl (\x y -> if (odd (fst x)) then ((fst x) + 1, snd x) else ((fst x) + 1, (snd x)++[y]))  (1,[]) x)

evenOnly :: [a] -> [a]
evenOnly = helper 1 where
    helper :: Integer -> [a] -> [a]
    helper _ [] = []
    helper c (x:xs) 
        | odd c = helper (c + 1) xs
        | otherwise = x:(helper (c + 1) xs) 

lastElem :: [a] -> a
lastElem = foldl1 (\x y -> y)  

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = (\x -> if (fst x > snd x) then Nothing else Just (snd x, (fst x, pred (snd x))))    

--------------------------------------------------

data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x:(fromList xs)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add (Zero) y = y
add x (Zero) = x
add (Suc x) (Suc y) = Suc (Suc (add x y))

mul :: Nat -> Nat -> Nat
mul (Zero) _ = Zero
mul _ (Zero) = Zero
mul x (Suc y) = add x (mul x y)

fac :: Nat -> Nat
fac (Zero) = (Suc Zero)
fac n@(Suc x) = mul n (fac x) 


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node left rigth) = 1 + max (height left) (height rigth) 

size :: Tree a -> Int
size (Leaf _) = 1
size (Node left rigth) = 1 + (size left) + (size rigth)


avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1,x)
    go (Node x y) = 
        case (go x, go y) of
            ((s1,c1),(s2,c2)) -> (s1 + s2, c1 + c2)



infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand (Val a) = Val a
expand ((e1 :+: e2) :*: e) = expand (e1 :*: e) :+: expand (e2 :*: e)
expand (e :*: (e1 :+: e2)) = expand (e :*: e1) :+: expand (e :*: e2)
expand ((Val a) :*: (Val b)) = (Val a) :*: (Val b)
expand ((Val a) :+: (Val b)) = (Val a) :+: (Val b)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = expand (expand e1 :*: expand e2)


--------------------------------------------------

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend (Maybe' (Just x)) (Maybe' (Just y)) = Maybe' (Just (mappend x y))    


class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((k,v):xs)) 
        | key == k = Just v
        | otherwise = lookup key (ListMap xs)
    insert key value (ListMap []) = ListMap ([(key,value)]) 
    insert key value (ListMap ((k,v):xs)) 
        | key == k = ListMap ((k,value):xs)
        | otherwise = ListMap (a:b) where
            ListMap b = insert key value (ListMap xs)  
            a = (k,v)
    delete key (ListMap []) = ListMap []
    delete key (ListMap ((k,v):xs)) 
        | key == k = ListMap xs
        | otherwise = ListMap (a:b) where
            ListMap b = delete key (ListMap xs) 
            a = (k,v)  


class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\x -> Nothing)
    lookup key (ArrowMap f) = f key
    insert key value (ArrowMap f) =
        case f key of
            Nothing -> ArrowMap (\x -> if (key == x) then (Just value) else (f x))
            Just _ -> ArrowMap (\x -> if (key == x) then (Just value) else (f x))
    delete key (ArrowMap f) = 
        case f key of
            Nothing -> ArrowMap f
            Just _ -> ArrowMap (\x -> if (key == x) then (Nothing) else (f x))
    fromList [] = empty
    fromList ((k,v):xs) = ArrowMap (\x -> if (k == x) then (Just v) else (f x)) where
        ArrowMap f = fromList xs

main :: IO()
main = print (expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5))



