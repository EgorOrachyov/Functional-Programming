module Evaluator (Term(..),Context,Name,Value,evaluate) where

    type Name =  String
    type Value = Int

    data Term = Const    Int
              | Variable Name
              | Add      Term Term
              | Apply    Term Term
              | Lambda   Name Term
              deriving (Show, Eq)

    type Context =   [(Name,Value)]
    type LambdaAcc = [(Name,Term)] 

    -- Name - String name in he equotation
    -- Term - !! Const Int or Lmbda Name Term
    -- DeBruijn indices are simulated by pushing context-associated touple of (lambda variable name,lambda body to substitute)

    evaluate :: Context -> Term -> Maybe Term
    
    evaluate context term = eval [] term where
        eval :: LambdaAcc -> Term -> Maybe Term

        eval acc lambda@(Lambda name body) = 
            case null acc of 
                True -> Just lambda
                False -> substitute acc lambda where
                    substitute :: LambdaAcc -> Term -> Maybe Term
                    substitute _ const@(Const i) = Just const
                    substitute accum variable@(Variable v) = eval accum variable
                    substitute accum (Add a b) = 
                        case (substitute accum a, substitute accum b) of 
                            (Just v1, Just v2) -> Just (Add v1 v2)
                            (_,_) -> Nothing
                    substitute accum (Apply a b) = 
                        case (substitute accum a, substitute accum b) of
                            (Just v1, Just v2) -> Just (Apply v1 v2)
                            (_,_) -> Nothing
                    substitute accum (Lambda name body) = 
                        case substitute newAccum body of 
                            Just term -> Just (Lambda name term)
                            Nothing -> Nothing 
                        where
                                newAccum :: LambdaAcc
                                newAccum = (name,(Variable name)):accum  

        eval _ const@(Const i) = Just const

        eval acc (Variable v) = result where
            findVariable :: Context -> Maybe Term
            findVariable [] = Nothing
            findVariable ((name,value):xs) = 
                if (v == name) 
                    then Just (Const value) 
                    else findVariable xs

            findLambda :: LambdaAcc -> Maybe Term
            findLambda [] = Nothing
            findLambda ((name,term):xs) = 
                if (v == name) 
                    then Just term
                    else findLambda xs 

            result :: Maybe Term
            result = case findLambda acc of
                Just v -> Just v 
                Nothing -> findVariable context                                
        
        eval _ (Add (Lambda _ _) _) = Nothing
        eval _ (Add _ (Lambda _ _)) = Nothing
        eval _ (Add (Const v1) (Const v2)) = Just (Const (v1 + v2))
        eval acc (Add e1 e2) = 
            case (eval acc e1, eval acc e2) of
                (Just v1, Just v2) -> eval acc (Add v1 v2)
                (_,_) -> Nothing

        eval acc (Apply lambda@(Lambda name1 body1) term@(Lambda name2 body2)) = result where
            result :: Maybe Term
            result = eval ((name1,term):acc) body1
        eval acc (Apply l1@(Lambda name body) term) = result where 
            result :: Maybe Term
            result = 
                case eval acc term of
                    Nothing -> Nothing
                    Just l2@(Lambda a b) -> eval acc (Apply l1 l2)
                    Just value@(Const i) -> eval ((name,value):acc) body 
        eval _ (Apply (Const _) _) = Nothing
        eval _ (Apply (Variable _) _) = Nothing          
        eval acc (Apply term1 term2) = 
            case (eval acc term1, eval acc term2) of
                (Just v1, Just v2) -> eval acc (Apply v1 v2)
                (_,_) -> Nothing  
