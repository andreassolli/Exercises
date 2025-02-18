data Expression
  = Constant Integer
  | Add      Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | Divide   Expression Expression
     deriving (Eq, Show, Read, Ord)

valuate :: Expression -> Integer
valuate (Constant n) = n
valuate (Add  e1 e2) = valuate e1 + valuate e2
valuate (Subtract e1 e2) = valuate e1 - valuate e2
valuate (Multiply e1 e2) = valuate e1  * valuate e2

main :: IO ()
main = do
    let expr1 = Add (Constant 5) (Multiply (Constant 3) (Constant 2))  
    let expr2 = Divide (Constant 10) (Constant 2) 
    let expr3 = Divide (Constant 10) (Constant 0) 
    print ("Expression 1 evaluates to: " ++ show (valuate expr1))
    print ("Expression 2 evaluates to: " ++ show (valuate expr2))
    print ("Expression 3 evaluates to: " ++ show (valuate expr3))  
