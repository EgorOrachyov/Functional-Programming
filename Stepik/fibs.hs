-- fibonacci :: Integer -> Integer
-- fibonacci n | n == 0    = 0
--             | n == 1    = 1
--             | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
--             | n == (-1) = 1
--             | n < -1    = fibonacci (n + 2) - fibonacci (n + 1)


fibonacci :: Integer -> Integer
fibonacci n | n == 0    = 0
            | n == 1    = 1
            | n > 1     = helper 1 1 2 where
                helper fp fn i | i == n = fn
                               | otherwise = helper fn (fp + fn) (i + 1) 
fibonacci n | n == (-1) = 1
            | n < -1    = helper 1 (-1) (-2) where
                helper fp fn i | i == n = fn
                               | otherwise = helper fn (fp - fn) (i - 1)



main :: IO()
main = print (fibonacci (7))           