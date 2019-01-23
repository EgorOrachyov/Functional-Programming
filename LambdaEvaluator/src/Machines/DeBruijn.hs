module DeBruijn (evaluateDeBruijn, DeBruijn, Context) where

    import Lambda

    data Result a = Success Output a
                  | Failure Error
                  deriving (Eq)

    instance (Show a) => (Show (Result a)) where
        show (Success out a) = "Output:\n" ++ out ++ "\nResult:\n " ++ (show a)
        show (Failure error) = "Error:\n"  ++ error

    ---------------------------------------------------------------------
    --                           Public part                           --
    ---------------------------------------------------------------------

    type Context = [DeBruijn]
    type DeBruijn = Term Index ()
    evaluateDeBruijn :: Context -> DeBruijn -> (Result DeBruijn)

    ---------------------------------------------------------------------
    --             Helpers and error-handling functions                --
    ---------------------------------------------------------------------

    newError :: Message -> Result DeBruijn
    newError m = Failure $ m ++ "\n"

    traceError :: DeBruijn -> Message -> Result DeBruijn
    traceError t m = Failure $ "at " ++ (show t) ++ "\n" ++ m

    evaluateLambda :: DeBruijn -> DeBruijn -> Result DeBruijn
    evaluateLambda term lambda@(Lambda _ body) = Success "" (substitute 0 body) where
        substitute :: Index -> DeBruijn -> DeBruijn 

        substitute index (Add term1 term2) = 
            Add (substitute index term1) (substitute index term2)

        substitute index (Apply term1 term2) = 
            Apply (substitute index term1) (substitute index term2)

        substitute index (Out term) =
            Out (substitute index term)

        substitute _ (Const v) =
            Const v

        substitute index (Lambda _ body) =
            Lambda () (substitute (index + 1) body)      

        substitute index (Variable i) = 
            case (i == index) of 
                True -> term
                False -> Variable (i - 1)

    ---------------------------------------------------------------------
    --                     evaluateDeBruijn Body                       --
    ---------------------------------------------------------------------

    evaluateDeBruijn c term@(Out t) = 
        case (evaluateDeBruijn c t) of 
            Success out a -> Success (out ++ " " ++ (show a) ++ "\n") a
            Failure error -> traceError term error

    evaluateDeBruijn _ const@(Const a) = Success "" const

    evaluateDeBruijn _ lambda@(Lambda _ _) = Success "" lambda

    evaluateDeBruijn c (Variable v) = findVariable 0 c where
        findVariable :: Index -> Context -> Result DeBruijn
        
        findVariable _ [] = 
            newError $ " Cannot find variable with index '" ++ (show v) ++ "'"
        
        findVariable i ((body):xs) = 
            case (i == v) of 
                True -> Success "" body
                False -> findVariable (i + 1) xs

    evaluateDeBruijn _ term@(Add l@(Lambda _ _) _) = 
        newError $ " Cannot add Lambda '" ++ (show l) ++ "' to something"

    evaluateDeBruijn _ term@(Add _ l@(Lambda _ _)) = 
        newError $ " Cannot add something to Lambda '" ++ (show l) ++ "'"

    evaluateDeBruijn _ (Add (Const a) (Const b)) = Success "" (Const (a + b))

    evaluateDeBruijn c term@(Add term1 term2) = 
        case (evaluateDeBruijn c term1, evaluateDeBruijn c term2) of
            (Success out1 a1, Success out2 a2) -> 
                case evaluateDeBruijn c (Add a1 a2) of
                    Success out a -> Success (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error
            (Failure error, Success _ _) -> traceError term error     
            (Success _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 

    evaluateDeBruijn _ (Apply term@(Const _) _) = 
        newError $ " Cannot apply something to const value '" ++ (show term) ++ "'"

    evaluateDeBruijn c (Apply lambda1@(Lambda _ body1) lambda2@(Lambda _ body2)) =
        case evaluateLambda lambda2 lambda1 of
            result@(Success _ (Lambda _ _)) -> result
            Success out term -> evaluateDeBruijn c term

    evaluateDeBruijn c (Apply lambda@(Lambda _ body) const@(Const v)) =
        case (evaluateLambda const lambda) of
            result@(Success out (Lambda _ term)) -> result
            Success out term -> evaluateDeBruijn c term

    evaluateDeBruijn c input@(Apply lambda@(Lambda _ _) term) = 
        case (evaluateDeBruijn c term) of
            Success out1 term1 -> 
                case evaluateDeBruijn c (Apply lambda term1) of
                    Success out2 term2 -> Success (out1 ++ out2) term2
                    Failure error -> traceError input error   
            Failure error -> traceError input error                      

    evaluateDeBruijn c term@(Apply term1 term2) = 
        case (evaluateDeBruijn c term1, evaluateDeBruijn c term2) of
            (Success out1 a1, Success out2 a2) ->
                case evaluateDeBruijn c (Apply a1 a2) of
                    Success out a -> Success (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error 
            (Failure error, Success _ _) -> traceError term error     
            (Success _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 


            