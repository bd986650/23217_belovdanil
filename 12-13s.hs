type VarName = String
data LambdaTerm
    = Lam VarName LambdaTerm    -- Abstraction
    | App LambdaTerm LambdaTerm -- Application
    | Var VarName               -- Variable
    
id' = Lam "x" (Var "x")

testTerm1 = Lam "n" (App (Var "f") (Var "n"))
testTerm2 = App testTerm1 (Lam "a" (Var "a"))
testTerm3 = App (Lam "x" (App (Var "x") (Var "x"))) (Var "n")

xx = Lam "x" (App (Var "x") (Var "x"))
testTerm4 = App xx xx

t = Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))
yComb = Lam "f" (App t t)

testTerm5 = App yComb (Var "foo")

-- Task 1
instance Show LambdaTerm where
    show (Var name) = name
    show (Lam var term) = "(λ" ++ var ++ "." ++ show term ++ ")"
    show (App term1 term2) = "(" ++ show term1 ++ "" ++ show term2 ++ ")"
    -- ps решил сделать без пробелов тк больше похоже на лямбды записанные рукой
    
-- Task 2
substitute :: LambdaTerm -> VarName -> LambdaTerm -> LambdaTerm
substitute (Var x) varName replacement
    | x == varName = replacement
    | otherwise = Var x
substitute (Lam x body) varName replacement
    | x == varName = Lam x body
    | otherwise = Lam x (substitute body varName replacement)
substitute (App t1 t2) varName replacement =
    App (substitute t1 varName replacement) (substitute t2 varName replacement)

betaReduce :: LambdaTerm -> LambdaTerm
betaReduce (App (Lam var body) arg) = substitute body var arg
betaReduce term = term

-- Task 3
canBeReduced :: LambdaTerm -> Bool
canBeReduced (App (Lam _ _) _) = True
canBeReduced _ = False

reduceSubterms :: LambdaTerm -> LambdaTerm
reduceSubterms (App (Lam var body) arg) = substitute body var arg
reduceSubterms term = term

eval :: LambdaTerm -> LambdaTerm
eval = evalHelp 0
    where
    evalHelp count term
        | canBeReduced term && count < maxCount = evalHelp (count+1) (reduceSubterms term)
        | otherwise = term
    maxCount = 1000
