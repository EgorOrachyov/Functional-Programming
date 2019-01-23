{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Ambiguous (
        Result,
        Printer(..)
    ) where

    import Lambda
    import Evaluable

    type Result = [(Output,Standard)]
    data Printer = ToShow Result

    instance Evaluable Result where
        stop = []
        
        use [] _ = []
        use list f = concatMap union list where
            union (out1,term1) = map addOutput $ f term1 where
                addOutput (out2,term2) = (out1 ++ out2,term2) 

        join left right = left ++ right
        
        output [] = []
        output ((out,term):xs) = 
            (out ++ " " ++ (show term) ++ ";\n",term):(output xs)

        success output term = [(output,term)]

        failure _ = []  


    instance  Show Printer where
        show (ToShow []) = ""
        show (ToShow ((out,term):xs)) =
            "Output:\n" ++ out ++
            "\nResult:\n " ++ (show term) ++ "\n\n" ++
            (show (ToShow xs))
