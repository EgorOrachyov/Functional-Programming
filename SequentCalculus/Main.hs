module Main where

    import Term
    import Deduce
    import Sequent
    import Visual

    f :: Term String
    f = ((Var "p" :& Var "q") :=> (Var "z" :| (Not (Var "z"))))

    main :: IO ()
    main = visual f