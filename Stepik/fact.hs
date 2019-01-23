doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)

main :: IO()
main = print (doubleFact 7, doubleFact 8, otherwise)