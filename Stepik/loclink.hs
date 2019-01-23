-- seq A --

seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n >= 3 = helper 1 2 3 2 where
                    helper a b c i | i == n = c
                                   | otherwise = helper (b) (c) (b + c - 2 * a) (i + 1)


-- decimal nums' sum --

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1) 
              | otherwise = helper (abs x) 0 0 where
                    helper 0 a c = (a,c)
                    helper x a c = helper (x `div` 10) (a + (x `mod` 10)) (c + 1)


-- integration (function,lower limit, upper limit) step = 1/1000 -- 

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a <= b = step * ((helper 1 0 (a + step)) + (f a + f b) / 2) where
        samples = 1000
        step = (b - a) / samples
        helper i a pos | i >= samples = a
                       | otherwise = helper (i + 1) (a + f pos) (pos + step)

integration f a b | otherwise = (-(integration f b a))                      


main :: IO()
main = print (integration (cos) ((-pi) / 2) (pi / 2))
