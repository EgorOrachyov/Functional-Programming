module Sequent where

    import Term

    infixl 5 ⊢
    (⊢) :: a -> b -> (a,b)
    (⊢) a b = (a,b)

    type Interpret a = [(a, Bool)]
    type Sequent a = ([Term a], [Term a])
    type Rule a = Term a -> [ Sequent a ]

    data Output a = Leaf (Sequent a)
                | Node (Output a) (Output a)
                | Level (Sequent a) (Output a)
                deriving (Show)

    -- Axiom for sequent calculus

    axiom :: (Eq a) => Term a -> Term a -> Bool
    axiom (Var a) (Var b) = (a == b)
    axiom _ _ = False

    -- Deduce introduction to antecedent

    antecedent :: Rule a
    antecedent (Not t)   = [ []    ⊢ [t] ]
    antecedent (a :| b)  = [ [a]   ⊢ []  , [b] ⊢ []  ]
    antecedent (a :& b)  = [ [a,b] ⊢ []  ] 
    antecedent (a :=> b) = [ [b]   ⊢ []  ,  [] ⊢ [a] ] 

    -- Deduce introduction to succedent

    succedent :: Rule a
    succedent (Not t)   = [ [t] ⊢ []    ]
    succedent (a :| b)  = [ []  ⊢ [a,b] ]
    succedent (a :& b)  = [ []  ⊢ [a]   , [] ⊢ [b] ]
    succedent (a :=> b) = [ [a] ⊢ [b]   ]
