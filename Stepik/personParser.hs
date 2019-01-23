import Data.Text

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }

sep1 :: Char -> Bool
sep1 c 
    | c == '\n' = True
    | otherwise = False 

sep2 :: Char -> Bool
sep2 c 
    | c == '=' || c == ' ' = True
    | otherwise = False 


parsePerson :: String -> Either Error Person
parsePerson s = undefined

main :: IO()
main = print ("abc" == "abc")