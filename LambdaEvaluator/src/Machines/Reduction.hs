module Reduction (evaluateReduction, DeBruijn, Context) where

    import Lambda

    data Result a = Success Index Output a
                  | Failure Error 
                  deriving (Eq)

    instance (Show a) => (Show (Result a)) where
        show (Success index out a) = 
            "Output:\n" ++ out ++ "\nReductions:\n " ++ (show index) ++ "\n\nResult:\n " ++ (show a)
        show (Failure error) = "Error:\n" ++ error               

    ---------------------------------------------------------------------
    --                           Public part                           --
    ---------------------------------------------------------------------

    type Context = [DeBruijn]
    type DeBruijn = Term Index ()
    evaluateReduction :: Context -> DeBruijn -> (Result DeBruijn)

    ---------------------------------------------------------------------
    --             Helpers and error-handling functions                --
    ---------------------------------------------------------------------

    newError :: Message -> Result DeBruijn
    newError m = Failure $ m ++ "\n"

    traceError :: DeBruijn -> Message -> Result DeBruijn
    traceError t m = Failure $ "at " ++ (show t) ++ "\n" ++ m

    evaluateLambda :: DeBruijn -> DeBruijn -> Result DeBruijn
    evaluateLambda term lambda@(Lambda _ body) = Success 1 "" (substitute 0 body) where
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
    --                    evaluateReduction Body                       --
    ---------------------------------------------------------------------

    evaluateReduction c term@(Out t) = 
        case (evaluateReduction c t) of 
            Success red out a -> Success (red + 1) (out ++ " " ++ (show a) ++ "\n") a
            Failure error -> traceError term error

    evaluateReduction _ const@(Const a) = Success 0 "" const

    evaluateReduction _ lambda@(Lambda _ _) = Success 0 "" lambda

    evaluateReduction c (Variable v) = findVariable 0 c where
        findVariable :: Index -> Context -> Result DeBruijn
        
        findVariable _ [] = 
            newError $ " Cannot find variable with index '" ++ (show v) ++ "'"
        
        findVariable i ((body):xs) = 
            case (i == v) of 
                True -> Success 1 "" body
                False -> findVariable (i + 1) xs

    evaluateReduction _ term@(Add l@(Lambda _ _) _) = 
        newError $ " Cannot add Lambda '" ++ (show l) ++ "' to something"

    evaluateReduction _ term@(Add _ l@(Lambda _ _)) = 
        newError $ " Cannot add something to Lambda '" ++ (show l) ++ "'"

    evaluateReduction _ (Add (Const a) (Const b)) = Success 1 "" (Const (a + b))

    evaluateReduction c term@(Add term1 term2) = 
        case (evaluateReduction c term1, evaluateReduction c term2) of
            (Success red1 out1 a1, Success red2 out2 a2) -> 
                case evaluateReduction c (Add a1 a2) of
                    Success red out a -> Success (red1 + red2 + red) (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error
            (Failure error, Success _ _ _) -> traceError term error     
            (Success _ _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 

    evaluateReduction _ (Apply term@(Const _) _) = 
        newError $ " Cannot apply something to const value '" ++ (show term) ++ "'"

    evaluateReduction c (Apply lambda1@(Lambda _ body1) lambda2@(Lambda _ body2)) =
        case evaluateLambda lambda2 lambda1 of
            result@(Success _ (Lambda _ _)) -> result
            Success out term -> evaluateReduction c term

    evaluateReduction c (Apply lambda@(Lambda _ body) const@(Const v)) =
        case (evaluateLambda const lambda) of
            result@(Success _ _ (Lambda _ _)) -> result
            Success red out term -> 
                case (evaluateReduction c term) of
                    (Success red1 out1 term1) -> Success (red + red1) (out ++ out1) term1
                    (Failure error) -> traceError term error

    evaluateReduction c input@(Apply lambda@(Lambda _ _) term) = 
        case (evaluateReduction c term) of
            Success red1 out1 term1 -> 
                case evaluateReduction c (Apply lambda term1) of
                    Success red2 out2 term -> Success (red1 + red2) (out1 ++ out2) term
                    Failure error -> traceError input error 
            Failure error -> traceError input error 

    evaluateReduction c term@(Apply term1 term2) = 
        case (evaluateReduction c term1, evaluateReduction c term2) of
            (Success red1 out1 a1, Success red2 out2 a2) ->
                case evaluateReduction c (Apply a1 a2) of
                    Success red out a -> Success (red1 + red2 + red) (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error 
            (Failure error, Success _ _ _) -> traceError term error     
            (Success _ _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 


