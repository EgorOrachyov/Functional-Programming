module Main where

import Evaluator

term :: Term
term = Apply (Lambda "x" (Add (Variable "x") (Variable "x"))) (Add (Variable "x") (Variable "y"))

apply :: Term
apply = Apply (Lambda "z" (Lambda "y" (Apply (Variable "z") (Variable "y")))) (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y"))))

test :: Term
test = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) (Lambda "y" (Apply (Variable "y") (Variable "y")))

lambda :: Term
lambda = Apply (Lambda "x" (Variable "x")) (Lambda "y" (Apply (Variable "y") (Variable "x")))

firstApply :: Term
firstApply = Apply (Lambda "x" (Apply (Lambda "y" (Variable "y")) (Variable "x"))) (Variable "u")

bug :: Term
bug = Apply (Apply (Lambda "x" (Lambda "y" (Add (Variable "y") (Variable "x") ) ) ) (Variable "x") ) (Variable "z") -- ) (Variable "z")

context :: Context
context = [("x",(-1)),("y",17),("z",(-5))]

main :: IO()
main = print (evaluate context bug)
