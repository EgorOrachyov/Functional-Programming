{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds     #-}

module Parser where

    import Data.Char
    import Control.Applicative hiding (many)


    newtype Parser a = Parser { apply :: String -> [(a, String)] }


    parse :: Parser a -> String -> a
    parse p = fst . head . apply p


    instance Functor Parser where
        fmap f p = Parser fun where
            fun input = [ (f a, s) | (a, s) <- apply p input ]    

    instance Applicative Parser where
        pure a = Parser fun where
            fun s = [(a,s)]
        pf <*> pv = Parser fun where
            fun input = [ (g a, s2) | (g, s1) <- apply pf input, (a, s2) <- apply pv s1 ]

    instance Alternative Parser where
        empty = Parser fun where
            fun _ = []
        pa <|> pb = Parser fun where
            fun input = 
                let 
                    r1 = apply pa input 
                in
                    if null r1
                    then apply pb input
                    else r1      


    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = Parser fun where
        fun ""     = []
        fun (x:xs) | f x = [(x,xs)]
                   | otherwise = []

    lower :: Parser Char
    lower = satisfy isLower 

    char :: Char -> Parser Char
    char c = satisfy (== c)

    digit :: Parser Int
    digit = digitToInt <$> satisfy isDigit

    anyChar :: Parser Char
    anyChar = Parser fun where
        fun ""     = []
        fun (x:xs) = [(x,xs)]        

    mult :: Parser Int
    mult = (*) <$> digit <* char '*' <*> digit

    many :: Parser a -> Parser [a]
    many p = (:) <$> p <*> many p <|> pure []

    many1 :: Parser a -> Parser [a]
    many1 p = (:) <$> p <*> many p

    nat :: Parser Int
    nat = f <$> (many1 digit) where
        f :: [Int] -> Int
        f x  = helper 0 x where
            helper r []     = r
            helper r (d:ds) = helper (r * 10 + d) ds







