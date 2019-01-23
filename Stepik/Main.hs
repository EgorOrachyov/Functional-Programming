module Main where

    import Parser

    main :: IO ()
    main = print ( apply ((*) <$> nat <* char '*' <*> nat) "16*128AAA")