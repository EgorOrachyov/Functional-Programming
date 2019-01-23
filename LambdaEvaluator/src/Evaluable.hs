module Evaluable (
        Standard,
        Context,
        Evaluable(..)
    ) where

    import Lambda

    type Standard = Term Name Name
    type Context  = [(Name,Standard)]

    class Evaluable r where

        -- Call eval to get result --

        eval    :: Context -> Standard -> r

        -- Should be defined by user --

        stop    :: r
        use     :: r -> (Standard -> r) -> r
        join    :: r -> r -> r
        output  :: r -> r
        success :: Output -> Standard -> r
        failure :: Error -> r       

        -- Implementation of evaluator core method eval --

        eval c (Stop) =
            stop

        eval c (Out term) =
            output $ eval c term 

        eval _ const@(Const c) = 
            success "" const

        eval _ lambda@(Lambda _ _) =
            success "" lambda

        eval c (Variable v) = find c where
            find [] = failure $ "Cannot find variable with name '" ++ v ++ "'"
            find ((name,term):xs) | name == v = success "" term
                                  | otherwise = find xs

        eval _ term@(Add (Lambda _ _) _) = 
            failure $ "Cannot add something to lambda in '" ++ (show term) ++ "'"

        eval _ term@(Add _ (Lambda _ _)) = 
            failure $ "Cannot add lambda to something in '" ++ (show term) ++ "'"                                    

        eval _ (Add (Const a) (Const b)) =
            success "" (Const (a + b))

        eval c (Add term1 term2) = 
            use (eval c term1) $ \left -> use (eval c term2) $
                                 \right -> eval c (Add left right)

        eval _ term@(Multiply (Lambda _ _) _) = 
            failure $ "Cannot multiply something to lambda in '" ++ (show term) ++ "'"

        eval _ term@(Multiply _ (Lambda _ _)) = 
            failure $ "Cannot multiply lambda to something in '" ++ (show term) ++ "'"                                    

        eval _ (Multiply (Const a) (Const b)) =
            success "" (Const (a * b))

        eval c (Multiply term1 term2) = 
            use (eval c term1) $ \left -> use (eval c term2) $
                                 \right -> eval c (Multiply left right)                          
                             
        eval c (Ambiguity term1 term2) = 
            join (eval c term1) (eval c term2)
                                        
        eval c term@(Apply (Const _) _) =
            failure $ "Cannot apply something to const value in '" ++ (show term) ++ "'"

        eval c (Apply (Lambda name body) lambda@(Lambda _ _)) =
            substituteCase c name lambda body

        eval c (Apply (Lambda name body) const@(Const _)) =
            substituteCase c name const body          

        eval c (Apply term1 term2) =
            use (eval c term1) $ \left -> use (eval c term2) $
                                 \right -> eval c (Apply left right) 


    -- Private helper functions --

    -- substitute :: (Name,Standard) -> Standard -> Standard
    substitute target (Add term1 term2) = 
        Add (substitute target term1) (substitute target term2)
    substitute target (Apply term1 term2) = 
        Apply (substitute target term1) (substitute target term2)
    substitute target (Multiply term1 term2) = 
        Multiply (substitute target term1) (substitute target term2)    
    substitute target (Ambiguity term1 term2) = 
        Ambiguity (substitute target term1) (substitute target term2)    
    substitute target (Out term) =
        Out (substitute target term)
    substitute _ (Const v) =
        Const v
    substitute target@(name,term) l@(Lambda n body) | (name == n) = l
                                                    | otherwise = Lambda n (substitute target body) 
    substitute (name,term) var@(Variable n) | (name == n) = term
                                            | otherwise = var

    -- substituteCase :: Context -> Name -> Standard -> Standard -> r
    substituteCase c name term body = 
        case substitute (name,term) body of
            result@(Lambda _ _) -> success "" result
            result -> eval c result                                 
