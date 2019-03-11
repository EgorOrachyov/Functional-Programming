module Term (Term(..)) where

    data Term a = Var a
                | Not (Term a)
                | (Term a) :=> (Term a)
                | (Term a) :&  (Term a)
                | (Term a) :|  (Term a)
                deriving (Eq)

    infixr 9 :=>

    instance (Show a) => Show (Term a) where
        show (Var a)   = show a
        show (Not t)   = "¬(" ++ (show t) ++ ")"
        show (a :=> b) = (show a) ++ " ⇒ " ++ (show b)
        show (a :&  b) = (show a) ++ " ∧ " ++ (show b)
        show (a :|  b) = (show a) ++ " ∨ " ++ (show b)