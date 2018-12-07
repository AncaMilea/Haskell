
data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

--1a
freeVariables :: Expr -> [Int]
freeVariables (Var x) = [x]
freeVariables (App x y) = freeVariables x ++ freeVariables y
freeVariables (Lam x ex) = [m | m<-freeVariables ex, m/=x]

--1b
rename :: Expr -> Int -> Int -> Expr
rename (Var x) y z = (Var x)
rename (App ex1 ex2) x y = (App (rename ex1 x y) (rename ex2 x y))
rename (Lam x ex) y z = if x==y 
	then (Lam z (renameWithin ex y z))
	else (Lam x (rename ex y z))

renameWithin (Var x) y z = if x==y then (Var z) else (Var x)
renameWithin (App ex1 ex2) x y = (App (renameWithin ex1 x y) (renameWithin ex2 x y))
renameWithin (Lam x ex) y z = if x==y 
	then (Lam x ex)
	else (Lam x (renameWithin ex y z))

--1c
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var x) (Var y) = x==y
alphaEquivalent (Var _) ex = False
alphaEquivalent (App ex1 ex2) (App ex3 ex4) =
	alphaEquivalent ex1 ex3 && alphaEquivalent ex2 ex4
alphaEquivalent (App _ _) ex = False
alphaEquivalent (Lam x ex1) (Lam y ex2) = if x==y 
	then alphaEquivalent ex1 ex2
	else alphaEquivalent ex1 (renameWithin ex2 y x)
alphaEquivalent (Lam _ _) ex = False

--1d
hasRedex :: Expr -> Bool
hasRedex (Var _) = False
hasRedex (Lam _ ex) = hasRedex ex
hasRedex (App (Lam _ _) _) = True
hasRedex (App ex1 ex2) = hasRedex ex1 || hasRedex ex2

--1e
substitute :: Expr -> Int -> Expr -> Expr
substitute (Var x) y e = if x==y then e else (Var x)
substitute (App ex1 ex2) x e = (App (substitute ex1 x e) (substitute ex2 x e))
substitute (Lam x ex) y e = if x==y
	then (Lam x ex)
	else if elem x (freeVariables e) 
		then substitute (rename (Lam x ex) x nxt) y e
		else (Lam x (substitute ex y e))
		where
			nxt = maximum (freeVariables e ++ freeVariables ex) + 1