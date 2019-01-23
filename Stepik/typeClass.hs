-- Type classes and interfaces --

class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
    toString True  = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where
    toString (a,b) = "(" ++ (toString a) ++ "," ++ (toString b) ++ ")"         

-- Standard class types --

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a = 
        if (doesEnrageGork a) 
        then  if (doesEnrageMork a) 
              then stomp (stab a)
              else stab a
        else  if (doesEnrageMork a)
              then stomp a
              else a           

class (Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc maxBound = minBound
  ssucc p = succ p

  spred :: a -> a
  spred minBound = maxBound 
  spred p = pred p            

{-

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc p = if (p == maxBound) then minBound else succ p

  spred :: a -> a
  spred p = if (p == minBound) then maxBound else pred p

-}

avg :: Int -> Int -> Int -> Double
avg a b c = (toDouble a + toDouble b + toDouble c) / 3.0 where 
    toDouble :: Integral a => a -> Double
    toDouble x = realToFrac x