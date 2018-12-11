data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)
-- Challenge 4

countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit = reduc e limit

reduc ::LamExpr->Int ->(Maybe Int, Maybe Int)
reduc a limit= ((leftRed a 0 limit),(rightRed a 0 limit))

leftRed :: LamExpr -> Int -> Int ->Maybe Int
leftRed a counter limit 
                     | counter > limit = Nothing
                     | a/=left = leftRed left (counter+1) limit
                     | otherwise = Just counter 
                    where
                        left = evalLeftCBV a


rightRed :: LamExpr -> Int -> Int ->Maybe Int
rightRed a counter limit 
                     | counter > limit = Nothing
                     | a/=right = rightRed right (counter+1) limit
                     | a==right && counter<=limit = Just counter 
                    where
                        right = evalRightCBV a

--I changed a bit the code given in the lectures
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
    x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
    x /=y && (free x e) = let x' = rename x in
        subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

rename x = 3*x
 --it evaluates to the right first and after it is done it does the left part
evalRightCBV :: LamExpr -> LamExpr
evalRightCBV (LamVar x) = (LamVar x)
evalRightCBV (LamAbs x e) = (LamAbs x (evalRightCBV e))
evalRightCBV (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalRightCBV (LamApp e@(LamAbs x e1) e2) = subst e1 x e2
evalRightCBV (LamApp e1 e@(LamVar x)) = LamApp (evalLeftCBV e1) e 
evalRightCBV (LamApp e1 e@(LamAbs x e2)) = LamApp (evalLeftCBV e1) e 
evalRightCBV (LamApp e1 e2) = LamApp e1 (evalRightCBV e2)

evalLeftCBV :: LamExpr -> LamExpr
evalLeftCBV (LamAbs x e) = (LamAbs x (evalLeftCBV e))
evalLeftCBV (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalLeftCBV (LamApp e@(LamVar x) e2) = LamApp e (evalRightCBV e2)
evalLeftCBV (LamApp e@(LamAbs x e1) e2) = subst e1 x e2
evalLeftCBV (LamApp e1 e2) = LamApp (evalLeftCBV e1) e2
evalLeftCBV (LamVar x) = (LamVar x)