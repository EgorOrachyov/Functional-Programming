{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Data.Char
import Prelude hiding (any)
import Control.Monad
import Control.Applicative hiding (many)

newtype Parser s a = Parser { parse :: [s] -> [(a,[s])] }

instance Functor (Parser s) where
    fmap f (Parser r) = Parser $ \s -> [(f x,xs) | (x,xs) <- r s]

instance Applicative (Parser s) where
    pure x = Parser $ \s -> [(x,s)]
    Parser f <*> Parser v = Parser $ \s -> [(g x, s'') | (g,s') <- f s, (x,s'') <- v s']

instance Alternative (Parser s) where
    empty = Parser $ \_ -> []
    Parser a <|> Parser b = Parser $ \s -> 
        let 
            r1 = a s
        in
            if null r1 
                then b s 
                else r1

instance Monad (Parser s) where
    return = pure
    Parser m >>= f = Parser $ \s -> [(y,s'') | (x,s') <- m s, (y,s'') <- parse (f x) s'] 

instance MonadPlus (Parser s) where
    mzero = empty
    Parser a `mplus` Parser b = Parser $ \s -> (a s) ++ (b s)     

runParser :: Parser s a -> [s] -> [(a,[s])]
runParser p s = parse p s

just :: Parser s a -> Parser s a
just (Parser p) = Parser $ \s -> [(x,xs) | (x,xs) <- p s, null xs]

item :: Parser s s
item = Parser $ \s ->
    case s of
        []     -> []
        (x:xs) -> [(x,xs)]

token :: Eq s => [s] -> Parser s [s]
token t = Parser p where
    p [] = []
    p xs | t == (take l xs) = [(t,drop l xs)]
         | otherwise = []

    l = length t    

oneOf :: Eq s => [s] -> Parser s s
oneOf l = Parser f where
    f [] = []
    f (x:xs) | elem x l  = [(x,xs)]
             | otherwise = []

any :: Eq s => [s] -> Parser s [s]
any l = (:) <$> oneOf l <*> any l <|> pure []

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f where
    f [] = []
    f (x:xs) | p x = [(x,xs)]
             | otherwise = []           

char :: Char -> Parser Char Char
char c = satisfy (== c)

skip :: (s -> Bool) -> Parser s [s]
skip f =  (\_ -> []) <$> many (satisfy (f))

spaces :: Parser Char [Char]
spaces = skip (==' ')

many :: Parser s a -> Parser s [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> many p