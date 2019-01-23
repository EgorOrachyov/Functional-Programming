module Ambiguity(
    Context,
    Standard,
    Printer(..),
    evaluateAmbiguous
    ) where

    import Lambda

    ---------------------------------------------------------------------
    --                           Public part                           --
    ---------------------------------------------------------------------

    type Context = [(Name,Standard)]
    type Standard = Term Name Name
    type EvalResult a = (Standard,Index,Output,a)
    type Result a =  [EvalResult a]
    data Printer a = ForOutput (Result a)
    evaluateAmbiguous :: Context -> Standard -> Result Standard

    ---------------------------------------------------------------------
    --             Helpers and error-handling functions                --
    ---------------------------------------------------------------------

    substitute :: (Name,Standard) -> Standard -> Standard
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
    substitute target@(name,term) l@(Lambda n body) =
        case (n == name) of
            True -> l
            False -> Lambda n (substitute target body)      
    substitute (name,term) var@(Variable n) = 
        case (name == n) of 
            True -> term
            False -> var


    caseOut :: Result Standard -> Result Standard
    caseOut [] = []
    caseOut ((rec,red,out,term):xs) = 
        (Out rec,red + 1,out ++ " " ++ (show term) ++ "\n", term):(caseOut xs)

    substituteCase :: Context -> Name -> Standard -> Standard -> Result Standard
    substituteCase c name term body = 
        case substitute (name,term) body of
            result@(Lambda _ _) -> [(Apply (Lambda name body) term,1,"",result)]
            result -> updateTuple (Apply) ((Lambda name body,1,"",result),(term,0,"",term)) $ evaluateAmbiguous c result        

    update :: EvalResult Standard -> Result Standard -> Result Standard        
    update _ [] = []
    update t@(Lambda name _,red1,out1,_) ((rec,red,out,term):xs) = 
        (Lambda name rec,red1 + red, out1 ++ out,term):(update t xs)     

    updateTuple :: (Standard -> Standard -> Standard) -> 
                   (EvalResult Standard,EvalResult Standard) -> 
                   Result Standard -> Result Standard        
    updateTuple _ _ [] = []
    updateTuple constr t@((rec1,red1,out1,_),(rec2,red2,out2,_)) ((_,red,out,term):xs) = 
        (constr rec1 rec2,red1 + red2 + red, out1 ++ out2 ++ out,term):(updateTuple constr t xs) 

    ---------------------------------------------------------------------
    --                         Printer show                            --
    ---------------------------------------------------------------------    

    instance (Show a) => Show (Printer a) where
        show (ForOutput []) = "\n"
        show (ForOutput ((rec,red,out,term):xs)) =
            "\nTerm:\n " ++ (show rec) ++
            "\nOutput:\n" ++ out ++ 
            "Reductions:\n " ++ (show red) ++ 
            "\nResult:\n " ++ (show term) ++ "\n" ++
            (show (ForOutput xs))

    ---------------------------------------------------------------------
    --                     evaluateAmbiguous Body                      --
    ---------------------------------------------------------------------

    evaluateAmbiguous c (Out t) = 
        caseOut $ evaluateAmbiguous c t

    evaluateAmbiguous _ const@(Const a) = 
        [(const,0,"",const)]

    evaluateAmbiguous _ lambda@(Lambda _ _) = 
        -- chooseAmbiguous :: Standard -> []
        [(lambda,0,"",lambda)]

    evaluateAmbiguous c (Variable n) = 
        findVariable c where
            findVariable :: Context -> Result Standard
            findVariable [] = []
            findVariable ((name,body):xs) = 
                case (name == n) of 
                    True -> [(Variable name,1,"",body)]
                    False -> findVariable xs

    evaluateAmbiguous _ (Add (Lambda _ _) _) = []

    evaluateAmbiguous _ (Add _ (Lambda _ _)) = []

    evaluateAmbiguous _ t@(Add (Const a) (Const b)) = 
        [(t,1,"",Const (a + b))] 

    evaluateAmbiguous c (Add term1 term2) = 
        concatMap unite [(x,y) | x <- evaluateAmbiguous c term1, y <- evaluateAmbiguous c term2] where
            unite :: (EvalResult Standard, EvalResult Standard) -> Result Standard
            unite t@((_,_,_,x),(_,_,_,y)) = 
                updateTuple (Add) t $ evaluateAmbiguous c (Add x y)

    evaluateAmbiguous _ (Multiply (Lambda _ _) _) = []
    
    evaluateAmbiguous _ (Multiply _ (Lambda _ _)) = []

    evaluateAmbiguous _ t@(Multiply (Const a) (Const b)) = 
        [(t,1,"",Const (a * b))] 

    evaluateAmbiguous c (Multiply term1 term2) = 
        concatMap unite [(x,y) | x <- evaluateAmbiguous c term1, y <- evaluateAmbiguous c term2] where
            unite :: (EvalResult Standard, EvalResult Standard) -> Result Standard
            unite t@((_,_,_,x),(_,_,_,y)) = 
                updateTuple (Multiply) t $ evaluateAmbiguous c (Multiply x y)            

    evaluateAmbiguous c (Ambiguity term1 term2) = 
        (evaluateAmbiguous c term1) ++ (evaluateAmbiguous c term2)
        -- concatMap unite [(x,y) | x <- evaluateAmbiguous c term1, y <- evaluateAmbiguous c term2] where
        --     unite :: (EvalResult Standard, EvalResult Standard) -> Result Standard
        --     unite t@((_,_,_,x),(_,_,_,y)) = 
        --         (updateTuple (Add) t (evaluateAmbiguous c (Add x y))) ++
        --         (updateTuple (Apply) t (evaluateAmbiguous c (Apply x y))) ++ 
        --         (updateTuple (Multiply) t (evaluateAmbiguous c (Multiply x y)))

    evaluateAmbiguous _ (Apply (Const _) _) = []

    evaluateAmbiguous c (Apply (Lambda name body) lambda@(Lambda _ _)) =
        substituteCase c name lambda body

    evaluateAmbiguous c (Apply (Lambda name body) const@(Const _)) =
        substituteCase c name const body

    evaluateAmbiguous c term@(Apply term1 term2) = 
        concatMap unite [(x,y) | x <- evaluateAmbiguous c term1, y <- evaluateAmbiguous c term2] where
            unite :: (EvalResult Standard, EvalResult Standard) -> Result Standard
            unite t@((_,_,_,x),(_,_,_,y)) = updateTuple (Apply) t $ evaluateAmbiguous c (Apply x y)





