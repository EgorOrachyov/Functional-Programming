import Effects
import Transformers
import Data.Void

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

f :: Void -> Int
f ~x = 1
v = f undefined

g :: () -> Int
g _ = 1
x = g ()

main :: IO ()
main = print ( foldl (+) 0 [1..1000000000] )
