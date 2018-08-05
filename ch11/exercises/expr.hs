data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add left right) = eval left + eval right

exprStr :: Expr -> String
exprStr (Lit a) = show a
exprStr (Add left right) = exprStr left ++ " + " ++ exprStr right
