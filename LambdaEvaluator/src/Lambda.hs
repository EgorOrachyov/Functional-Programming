module Lambda (
      Name,
      Value,
      Index,
      Output,
      Error,
      Message,
      Term(..)
  ) where
    
    type Name    = String
    type Value   = Int
    type Index   = Int
    type Output  = String
    type Error   = String
    type Message = String

    data Term a b = Const     Value 
                  | Variable  a
                  | Add       (Term a b) (Term a b)
                  | Multiply  (Term a b) (Term a b)
                  | Apply     (Term a b) (Term a b)
                  | Ambiguity (Term a b) (Term a b)
                  | Lambda    b          (Term a b)
                  | Stop
                  | Out       (Term a b)
                  deriving (Eq)

    instance (Show a, Show b) => Show (Term a b) where
        show (Const i) = show i
        show (Variable v) =  '•':(show v)
        show (Add t1 t2) = "(" ++ (show t1) ++ " + " ++ (show t2) ++ ")"
        show (Multiply t1 t2) = "(" ++ (show t1) ++ " * " ++ (show t2) ++ ")"
        show (Apply t1 t2) = "(" ++ (show t1) ++ ")(" ++ (show t2) ++ ")"
        show (Ambiguity t1 t2) = "(" ++ (show t1) ++ " [?] " ++ (show t2) ++ ")"
        show (Lambda b body) = "λ" ++ (show b) ++ "." ++ (show body)
        show (Out t) = '↑':(show t)