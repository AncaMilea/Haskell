{-For	 this	 challenge, you	 will	 program	 beta	 reduction	 of	 a	 lambda	 expression	 using	 two	 different	
strategies.	The	 first	 one	 repeatedly	 takes	 the	 innermost	 leftmost	reducible	expression	 (redex) and	
applies	 beta	 conversion	 to	 that.	 	The	 second	 repeatedly	 takes	 the	 innermost	rightmost	 redex	and	
applies	 beta	 conversion	 to	 that.	 	 Note	 that	 in	 the	 lambda	 calculus,	 reduction	 under	 lambda	 is	
allowed.	 	 Your	 countReds	function	should	 take a	 lambda	 calculus	 expression	 and	 return a	 pair	 of	
values.	 	The	 first	 component	 of	 this	 pair	gives	the	 number	 of	 reduction	 steps	 required	 before	 the	
first	 strategy	 converges,	 and	 the	 number	 of	 reduction	 steps	 required	 before	 the	 second	 strategy	
converges.		Your	function	has	a second	parameter	which	limits	the	maximum	number	of	reductions	
to	attempt.		If	strictly	more	than	this	number	are	required	by	one	strategy	or	other,	the	countReds
function	returns Nothing instead	of	a	number	for	the	corresponding	component	of	the	pair.
For	 example,	 the	 lambda	 expression	 whose	 concrete	 syntax	 is	 shown	 below	 has	 two	 reducible	
expressions,	which	are	indicated	by	the	underlined	text	above:
						(λx	->	λy	->	x) z ((λt	->	t) u)
This	expression	beta	reduces	using	innermost	leftmost	reduction	as	follows:
						(λx	->	λy	->	x) z ((λt	->	t)	u)	
						(λy	->	z)	((λt	->	t) u)
						z
This	expression	reduces	using	innermost	rightmost	reduction	as	follows:
						(λx	->	λy	->	x)	z ((λt	->	t) u))	
						(λx	->	λy	->	x)	z u	
						(λy	->	z)	u	
						z
As	the	first	sequence	has	2 reductions,	and	the	second	has	3,	countReds returns	(Just	2,	Just	3)	in	this	
case,	provided	the	limit	is	3	or	more.		Some	examples	of	this	function	are:-}
-- Lambda	expression Limit Result
-- λx	->	(λy	->	y) 0 (Just	0,	Just	0)
-- (λx	->	x)(λy	->	y) 1 (Just	1,	Just	1)
-- (λx	->	λy	->	x)	z	((λt	->	t)	u) 10 (Just	2,	Just	3)										
-- (λx	->	λy	->	x)	z	((λt	->	t)	u) 2 (Just	2,	Nothing)									-- limit	exceeded	for	rightmost
-- (λx	->	λy	->	x)	z	((λt	->	t)	u) 1 (Nothing,	Nothing)					-- limit	exceeded	for	both
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)
-- Challenge 4

countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit = reduction e 0 limit

reduction :: LamExpr -> Int -> Int -> (Maybe Int, Maybe Int)
reduction a counter limit | counter > limit               = (Nothing, Nothing)
                          | a /= leftRed && a/= rightRed  = (fst (reduction leftRed (counter+1) limit), snd (reduction rightRed (counter+1) limit))
                          | a /= leftRed                  = (fst (reduction leftRed (counter+1) limit), Just counter)
                          | a /= rightRed                 = (Just counter, snd (reduction rightRed (counter+1) limit))
                          | otherwise                     = (Just counter, Just counter)
                         where 
                            leftRed = evalLeftCBV a 
                            rightRed = evalRightCBV a 


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
-- evalRightCBV (LamApp e1 e@(LamAbs x e2)) = LamApp (evalLeftCBV e1) e 
evalRightCBV (LamApp e1 e2) = LamApp e1 (evalRightCBV e2)

evalLeftCBV :: LamExpr -> LamExpr
evalLeftCBV (LamAbs x e) = (LamAbs x (evalLeftCBV e))
evalLeftCBV (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalLeftCBV (LamApp e@(LamVar x) e2) = LamApp e (evalRightCBV e2)
evalLeftCBV (LamApp e@(LamAbs x e1) e2) = subst e1 x e2
evalLeftCBV (LamApp e1 e2) = LamApp (evalLeftCBV e1) e2
evalLeftCBV (LamVar x) = (LamVar x)
