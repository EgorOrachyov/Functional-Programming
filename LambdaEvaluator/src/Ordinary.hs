module Ordinary (
        Result
    ) where

    import Lambda
    import Evaluable

    data Result = Success Output Standard
                | Failure Error
                deriving (Eq)

    instance Evaluable Result where
        stop = Failure "Met Stop identifier"            

        use r@(Failure _) _ = r
        use (Success out1 term1) f = 
            case f term1 of
                Success out2 term2 -> Success (out1 ++ out2) term2
                r@(Failure _) -> r

        join _ _ = Failure "Ambiguous constructors are prohibited"

        output r@(Failure _) = r
        output (Success out term) = 
            Success (out ++ " " ++ (show term) ++ ";\n") term

        success output term = Success output term
        
        failure message = Failure message       

    
    instance Show Result where
        show (Success out a) = 
            "Output:\n" ++ out ++ 
            "Result:\n " ++ (show a)
        show (Failure error) = 
            "Error:\n"  ++ error    