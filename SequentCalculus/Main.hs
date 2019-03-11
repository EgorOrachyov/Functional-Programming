module Main where

    import Term
    import Deduce
    import Sequent
    import Visual

    f :: Term String
    f = (Var "a" :=> Var "b" :=> Var "c") :=> (Var "a" :=> Var "b") :=> (Var "a" :=> Var "c")

    main :: IO ()
    main = visual f