module Deduce where

    import Term
    import Sequent

    -- Main deduce funcion (Return Deduce tree, if term is valid formula,
    -- or interpretation (part of that), in which it has False value)

    deduce :: (Eq a) => Term a -> Either (Output a) (Interpret a)
    deduce t = unapply [] [] [] [t]

    -- Evaluation

    unapply :: (Eq a) => [Term a] -> [Term a] -> [Term a] -> [Term a] ->  Either (Output a) (Interpret a)
    
    unapply a_v [] s_v [] = 
        if (null list) then Right $ interpret else Left $ Leaf (a_v ⊢ s_v) where
            list = [(axiom x y) | x <- a_v, y <- s_v]
            interpret = [(name x, True) | x <- a_v] ++ [(name x, False) | x <- s_v]                    

    unapply a_v l@(a:as) s_v ss = 
        case antecedent a of
            
            [(left,right)] -> 
                case unapply (a_v ++ na_v) (as ++ na_s) (s_v ++ ns_v) (ss ++ ns_s) of
                    Left s -> Left $ Level (a_v ++ l, s_v ++ ss) s
                    Right interpret -> Right interpret
                where
                    (na_v,na_s) = sort left
                    (ns_v,ns_s) = sort right

            [(left1,right1), (left2,right2)] ->
                case unapply (a_v ++ na_v1) (as ++ na_s1) (s_v ++ ns_v1) (ss ++ ns_s1) of
                    Left ls -> case unapply (a_v ++ na_v2) (as ++ na_s2) (s_v ++ ns_v2) (ss ++ ns_s2) of
                        Left rs -> Left $ Node rs ls
                        Right interpret -> Right interpret
                    Right interpret -> Right interpret

                where
                    (na_v1,na_s1) = sort left1
                    (ns_v1,ns_s1) = sort right1
                    (na_v2,na_s2) = sort left2
                    (ns_v2,ns_s2) = sort right2

    unapply a_v [] s_v l@(s:ss) = 
        case succedent s of
            
            [(left,right)] -> 
                case unapply (a_v ++ na_v) na_s (s_v ++ ns_v) (ss ++ ns_s) of
                    Left s -> Left $ Level (a_v, s_v ++ l) s
                    Right interpret -> Right interpret
                where
                    (na_v,na_s) = sort left
                    (ns_v,ns_s) = sort right

            [(left1,right1), (left2,right2)] ->
                case unapply (a_v ++ na_v1) na_s1 (s_v ++ ns_v1) (ss ++ ns_s1) of
                    Left ls -> case unapply (a_v ++ na_v2) na_s2 (s_v ++ ns_v2) (ss ++ ns_s2) of
                        Left rs -> Left $ Node rs ls
                        Right interpret -> Right interpret
                    Right interpret -> Right interpret

                where
                    (na_v1,na_s1) = sort left1
                    (ns_v1,ns_s1) = sort right1
                    (na_v2,na_s2) = sort left2
                    (ns_v2,ns_s2) = sort right2                  

    -- Helper functions

    name :: Term a -> a
    name (Var s) = s

    sort :: [Term a] -> ([Term a], [Term a])
    sort [] = ([],[])
    sort (t@(Var x):xs) = (t:vs,ts) where
        (vs,ts) = sort xs
    sort (t:xs) = (vs,t:ts) where
        (vs,ts) = sort xs
