module Standard (evaluateStandard, Standard, Context) where

    import Lambda

    data Result a = Success Index Output a
                  | Failure Error 
                  deriving (Eq)

    instance (Show a) => (Show (Result a)) where
        show (Success index out a) = 
            "Output:\n" ++ out ++ "\nReductions:\n " ++ (show index) ++ "\n\nResult:\n " ++ (show a)
        show (Failure error) = "Error:\n"  ++ error 

    ---------------------------------------------------------------------
    --                           Public part                           --
    ---------------------------------------------------------------------

    type Context = [(Name,Standard)]
    type Standard = Term Name Name
    evaluateStandard :: Context -> Standard -> (Result Standard)

    ---------------------------------------------------------------------
    --             Helpers and error-handling functions                --
    ---------------------------------------------------------------------

    newError :: Message -> Result Standard
    newError m = Failure $ m ++ "\n"

    traceError :: Standard -> Message -> Result Standard
    traceError t m = Failure $ "at " ++ (show t) ++ "\n" ++ m

    evaluateLambda :: (Name,Standard) -> Standard -> Result Standard
    evaluateLambda (name,term) lambda@(Lambda _ body) = Success 1 "" (substitute body) where
        substitute :: Standard -> Standard 

        substitute (Add term1 term2) = 
            Add (substitute term1) (substitute term2)

        substitute (Apply term1 term2) = 
            Apply (substitute term1) (substitute term2)

        substitute (Out term) =
            Out (substitute term)

        substitute (Const v) =
            Const v

        substitute l@(Lambda n body) =
            case (n == name) of
                True -> l
                False -> Lambda n (substitute body)      

        substitute (Variable n) = 
            case (name == n) of 
                True -> term
                False -> Variable n

    ---------------------------------------------------------------------
    --                     evaluateStandard Body                       --
    ---------------------------------------------------------------------

    evaluateStandard c term@(Out t) = 
        case (evaluateStandard c t) of 
            Success red out a -> Success (red + 1) (out ++ " " ++ (show a) ++ "\n") a
            Failure error -> traceError term error

    evaluateStandard _ const@(Const a) = Success 0 "" const

    evaluateStandard _ lambda@(Lambda _ _) = Success 0 "" lambda

    evaluateStandard c (Variable n) = findVariable c where
        findVariable :: Context -> Result Standard
        
        findVariable [] = 
            newError $ " Cannot find variable with name '" ++ (show n) ++ "'"
        
        findVariable ((name,body):xs) = 
            case (name == n) of 
                True -> Success 1 "" body
                False -> findVariable xs

    evaluateStandard _ term@(Add l@(Lambda _ _) _) = 
        newError $ " Cannot add Lambda '" ++ (show l) ++ "' to something"

    evaluateStandard _ term@(Add _ l@(Lambda _ _)) = 
        newError $ " Cannot add something to Lambda '" ++ (show l) ++ "'"

    evaluateStandard _ (Add (Const a) (Const b)) = Success 1 "" (Const (a + b))

    evaluateStandard c term@(Add term1 term2) = 
        case (evaluateStandard c term1, evaluateStandard c term2) of
            (Success red1 out1 a1, Success red2 out2 a2) -> 
                case evaluateStandard c (Add a1 a2) of
                    Success red out a -> Success (red1 + red2 + red) (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error
            (Failure error, Success _ _ _) -> traceError term error     
            (Success _ _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 

    evaluateStandard _ (Ambiguity term1 term2) = 
        newError $ " Ambiguous operator for terms '" ++ (show term1) ++ "' and '" ++ (show term2) ++ ";"

    evaluateStandard _ (Apply term@(Const _) _) = 
        newError $ " Cannot apply something to const value '" ++ (show term) ++ "'"

    evaluateStandard c (Apply lambda1@(Lambda name body1) lambda2@(Lambda _ _)) =
        case (evaluateLambda (name,lambda2) lambda1) of
            result@(Success _ _ (Lambda _ _)) -> result
            Success red out term -> 
                case (evaluateStandard c term) of
                    (Success red1 out1 term1) -> Success (red + red1) (out ++ out1) term1
                    (Failure error) -> traceError term error

    evaluateStandard c (Apply lambda@(Lambda name body) const@(Const _)) =
        case (evaluateLambda (name,const) lambda) of
            result@(Success _ _ (Lambda _ _)) -> result
            Success red out term -> 
                case (evaluateStandard c term) of
                    (Success red1 out1 term1) -> Success (red + red1) (out ++ out1) term1
                    (Failure error) -> traceError term error

    evaluateStandard c input@(Apply lambda@(Lambda _ _) term) = 
        case (evaluateStandard c term) of
            Success red1 out1 term1 -> 
                case evaluateStandard c (Apply lambda term1) of
                    Success red2 out2 term -> Success (red1 + red2) (out1 ++ out2) term
                    Failure error -> traceError input error 
            Failure error -> traceError input error            

    evaluateStandard c term@(Apply term1 term2) = 
        case (evaluateStandard c term1, evaluateStandard c term2) of
            (Success red1 out1 a1, Success red2 out2 a2) ->
                case evaluateStandard c (Apply a1 a2) of
                    Success red out a -> Success (red1 + red2 + red) (out1 ++ out2 ++ out) a
                    Failure error -> traceError term error 
            (Failure error, Success _ _ _) -> traceError term error     
            (Success _ _ _, Failure error) -> traceError term error
            (Failure error1, Failure error2) -> traceError term  (error1 ++ error2) 



