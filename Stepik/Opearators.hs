import Data.Char

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = 
	if ((isDigit x) && (isDigit y))
	 then (digitToInt x) * 10 + (digitToInt y) 
	 else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

main :: IO()
main = print (dist (1,1) (5,4))