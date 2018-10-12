import Evaluator
import Test.HUnit
import System.IO.Unsafe

context :: Context
context = [("x",(-1)),("y",17),("z",(-5))]

term1 :: Term
term1 = Apply (Lambda "x" (Add (Variable "x") (Variable "x"))) (Add (Variable "x") (Variable "y"))
test1  = TestCase (assertEqual "λx.x+x)(x + y)" (Nothing) (evaluate context term1))

term2 :: Term
term2 = Apply (Lambda "z" (Lambda "y" (Apply (Variable "z") (Variable "y")))) (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y"))))
test2  = TestCase (assertEqual "(λz.λy.zy)(λx.λy.xy)" (Nothing) (evaluate context term2))

term3 :: Term
term3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) (Lambda "y" (Apply (Variable "y") (Variable "y")))
test3  = TestCase (assertEqual "(λx.λy.xy)(λy.yy)" (Nothing) (evaluate context term3))

term4 :: Term
term4 = Apply (Lambda "x" (Variable "x")) (Lambda "y" (Apply (Variable "y") (Variable "x")))
test4  = TestCase (assertEqual "(λx.x)(λy.yx)" (Nothing) (evaluate context term4))

term5 :: Term
term5 = Apply (Lambda "x" (Apply (Lambda "y" (Variable "y")) (Variable "x"))) (Variable "u")
test5  = TestCase (assertEqual "(λx.(λy.yx))(u)" (Nothing) (evaluate context term5))

term6 :: Term
term6 = Apply (Apply (Lambda "x" (Lambda "y" (Add (Variable "y") (Variable "x") ) ) ) (Variable "x") ) (Variable "z")
test6  = TestCase (assertEqual "((λx.λy.y + x)(x))(z)" (Nothing) (evaluate context term6))

term7 :: Term
term7 = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Lambda "x" (Add (Variable "x") (Variable "x")))
test7  = TestCase (assertEqual "(λx.xx)(λx.x+x)" (Nothing) (evaluate context term7))

term8 :: Term
term8 = Apply (Lambda "x" (Add (Variable "x") (Variable "x"))) (Lambda "x" (Add (Variable "x") (Variable "x")))
test8  = TestCase (assertEqual "(λx.x+x)(λx.x+x)" (Nothing) (evaluate context term8))

term9 :: Term
term9  = Apply (Lambda "x" (Variable "x")) (Lambda "x" (Add (Add (Variable "x") (Variable "x")) (Variable "x"))) 
test9 = TestCase (assertEqual "(λx.x)(λx.x + x + x)" (Just (Const 51)) (evaluate context term9))

term10 :: Term
term10  = Apply (Lambda "x" (Apply (Variable "x") (Variable "y"))) (Lambda "x" (Add (Add (Variable "x") (Variable "x")) (Variable "x"))) 
test10 = TestCase (assertEqual "(λx.xy)(λx.x + x + x)" (Just (Const 51)) (evaluate context term10))

tests = TestList [
    TestLabel "test1"  test1,
    TestLabel "test2"  test2,
    TestLabel "test3"  test3,
    TestLabel "test4"  test4,
    TestLabel "test5"  test5,
    TestLabel "test6"  test6,
    TestLabel "test7"  test7,
    TestLabel "test8"  test8,
    TestLabel "test9"  test9,
    TestLabel "test10" test10
    ]

main :: IO ()
main = print (unsafePerformIO (runTestTT tests))
        