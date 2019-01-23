import Text.Parsec
import Text.Parsec.Char

getList :: Parsec String u [String]
getList = many1 digit `sepBy` (char ';')

main :: IO ()
main = print (parse getList "" "12632;;3223;2332;23;2332;2233;")