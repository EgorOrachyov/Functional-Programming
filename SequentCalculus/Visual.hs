module Visual (visual) where

    import Term
    import Deduce
    import Sequent

    -- Pretty deduction / result printing --  

    visual :: (Show a, Eq a) => Term a -> IO ()
    visual t = do
        case deduce t of
            Left sequents -> outputbyline sequents
            Right interpretation -> print interpretation

    outputbyline :: (Show a) => [Sequent a] -> IO ()
    outputbyline [] = do putStrLn ""
    outputbyline ((left,right):xs) = do
        showterms left
        putStr " ⊢ "
        showterms right
        putStrLn ""
        outputbyline xs

    showterms :: (Show a) => [Term a] -> IO ()
    showterms [] = putStr ""
    showterms (t:[]) = putStr . show $ t
    showterms (t:ts) = do
        putStr . show $ t
        putStr " , "
        showterms ts 