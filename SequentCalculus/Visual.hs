module Visual (visual)  where

    import Term
    import Deduce
    import Sequent

    -- Pretty deduction / result printing --  

    visual :: (Show a, Eq a) => Term a -> IO ()
    visual t = do
        case deduce t of
            Left sequents -> outputbyline sequents "+ "
            Right interpretation -> print interpretation

    outputbyline :: (Show a) => Output a -> String-> IO ()
    outputbyline (Leaf (left,right)) move = do
        putStr move
        showterms left
        putStr " ⊢ "
        showterms right
        putStrLn ""
    outputbyline (Level (left,right) xs) move = do
        putStr move
        showterms left
        putStr " ⊢ "
        showterms right
        putStrLn ""
        outputbyline xs move 
    outputbyline (Node left right) move = do
        outputbyline left ("+-" ++ move) 
        putStrLn ("+-" ++ move ++ "variant")
        outputbyline right ("+-" ++ move)     

    showterms :: (Show a) => [Term a] -> IO ()
    showterms [] = putStr ""
    showterms (t:[]) = putStr . show $ t
    showterms (t:ts) = do
        putStr . show $ t
        putStr " , "
        showterms ts 