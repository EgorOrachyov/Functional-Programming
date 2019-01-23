module Main where

import Lambda

{-

import Reduction

context :: Context
context = [(Lambda () (Variable 0)), (Lambda () (Add (Variable 0) (Variable 0)))]

term1 :: DeBruijn
term1 = (Apply (Apply (Out (Variable 0)) (Variable 1)) (Apply (Out (Lambda () (Variable 0))) (Const 11)))
test1 = evaluateReduction context term1  

-}

{-

import Standard

context :: Context
context = [("x", (Const 12)), ("y", (Add (Const 11) (Const (-1))))]

term1 :: Standard
term1 = Apply (Out (Lambda "x" (Apply (Variable "x") (Variable "y")))) (Lambda "x" (Add (Add (Variable "x") (Variable "x")) (Variable "x")))
test1 = evaluateStandard context term1  

term2 :: Standard
term2 = (Out (Add (Ambiguity (Const 2) (Const 3)) (Variable "x")))
test2 = evaluateStandard context term2  

-}

{-

import DeBruijn

context :: Context
context = [(Const 12), (Add (Const 11) (Const (-1)))]

term1 :: DeBruijn
term1 = Apply (Out (Lambda () (Apply (Variable 0) (Variable 2)))) (Lambda () (Add (Add (Variable 0) (Variable 0)) (Variable 0)))
test1 = evaluateDeBruijn context term1  

term2 :: DeBruijn
term2 = Apply (Lambda () (Apply (Variable 0) (Variable 0))) (Lambda () (Apply (Variable 0) (Variable 0)))
test2 = evaluateDeBruijn context term2

-}


{-

import Ambiguity

context :: Context
context = 
    [ ("x", (Const 12))
    , ("y", (Add (Const 11) (Const (-1))))
    , ("z", (Lambda "x" (Out (Ambiguity (Const 3) (Variable "y")))))
    , ("k", (Ambiguity (Const 3) (Const 10)))
    , ("m", (Lambda "x" (Variable "x"))) 
    ]

term1 :: Standard
term1 = Apply (Out (Lambda "x" (Apply (Variable "x") (Variable "y")))) (Lambda "x" (Add (Add (Variable "x") (Variable "x")) (Variable "x")))
test1 = evaluateAmbiguous context term1  

term2 :: Standard
term2 = (Out (Add (Ambiguity (Const 2) (Const 3)) (Variable "x")))
test2 = evaluateAmbiguous context term2 

term3 :: Standard
term3 = (Out (Add (Ambiguity (Variable "x") (Variable "y")) (Ambiguity (Variable "z") (Variable "k"))))
test3 = evaluateAmbiguous context term3

term4 :: Standard
term4 = (Out (Ambiguity (Ambiguity (Variable "x") (Variable "y")) (Ambiguity (Variable "x") (Variable "y"))))
test4 = evaluateAmbiguous context term4

term5 :: Standard
term5 = Apply (Out (Lambda "x" (Ambiguity (Variable "m") (Variable "x") ) ) ) (Ambiguity (Variable "y") (Variable "y"))
test5 = evaluateAmbiguous context term5

term6 :: Standard
term6 = Apply (Lambda "x" (Add (Variable "x") (Variable "x"))) (Const 17)
test6 = evaluateAmbiguous context term6

term7 :: Standard
term7 = (Out (Ambiguity (Apply (Lambda "x" (Variable "x")) (Variable "x")) (Apply (Lambda "x" (Variable "x")) (Variable "y"))))
test7 = evaluateAmbiguous context term7

--}

--{-

import Evaluable
import Ambiguous

context :: Context
context = 
    [ ("x", (Const 12))
    , ("y", (Add (Const 11) (Const (-1))))
    , ("z", (Lambda "x" (Out (Ambiguity (Const 3) (Variable "y")))))
    , ("k", (Ambiguity (Const 3) (Const 10)))
    , ("m", (Lambda "x" (Variable "x"))) 
    ]

term1 :: Standard
term1 = Apply (Lambda "x" (Add (Variable "x") (Variable "x"))) (Const 17)
test1 = eval context term1 :: Result

term2 :: Standard
term2 = (Out (Ambiguity (Apply (Lambda "x" (Variable "x")) (Variable "x")) (Apply (Lambda "x" (Variable "x")) (Variable "y"))))
test2 = eval context term2 :: Result

term3 :: Standard
term3 = (Out (Ambiguity (Ambiguity (Variable "x") (Variable "y")) (Ambiguity (Variable "x") (Variable "y"))))
test3 = eval context term3 :: Result

---}

{-

import Evaluable
import Ordinary

context :: Context
context = 
    [ ("x", (Const 12))
    , ("y", (Add (Const 11) (Const (-1))))
    , ("z", (Lambda "x" (Out (Ambiguity (Const 3) (Variable "y")))))
    , ("k", (Ambiguity (Const 3) (Const 10)))
    , ("m", (Lambda "x" (Variable "x"))) 
    ]

term1 :: Standard
term1 = Apply (Out (Lambda "x" (Apply (Variable "x") (Variable "y")))) (Lambda "x" (Add (Add (Variable "x") (Variable "x")) (Variable "x")))
test1 = eval context term1 :: Result

term2 :: Standard
term2 = (Out (Add (Ambiguity (Const 2) (Const 3)) (Variable "x")))
test2 = eval context term2 :: Result 

term3 :: Standard
term3 = (Out (Ambiguity (Apply (Lambda "x" (Variable "x")) (Variable "x")) (Apply (Lambda "x" (Variable "x")) (Variable "y"))))
test3 = eval context term3 :: Result

term4 :: Standard
term4 = Out (Stop)
test4 = eval context term4 :: Result

-}

main :: IO()
main = print (test2)



