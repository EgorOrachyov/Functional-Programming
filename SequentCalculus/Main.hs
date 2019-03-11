module Main where

    import Term
    import Deduce
    import Sequent
    import Visual

    f :: Term String
    f = Var "p" :=> (Var "p" :| Var "q")

    main :: IO ()
    main = print ()