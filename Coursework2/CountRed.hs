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
-- count reductions using two different strategies 
-- countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- -- replace the definition below with your solution
-- countReds e limit = (Nothing, Nothing)

-- mult :: Int -> Int -> Int
-- mult = λ x ->λ y -> x * y

countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit = nextBoth e 0 limit

nextBoth :: LamExpr -> Int -> Int -> (Maybe Int, Maybe Int)
nextBoth a current limit | current > limit          = (Nothing, Nothing)
                         | a /= nextL && a/= nextR  = (fst $ nextBoth nextL (current+1) limit, snd $ nextBoth nextR (current+1) limit)
                         | a /= nextL               = (fst $ nextBoth nextL (current+1) limit, Just current)
                         | a /= nextR               = (Just current, snd $ nextBoth nextR (current+1) limit)
                         | otherwise                = (Just current, Just current)
                         where 
                            nextL = eval1cbn a 
                            nextR = eval2cbn a 


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

rename x = 2*x

eval1cbn :: LamExpr -> LamExpr
eval1cbn (LamAbs x e) = (LamAbs x e)
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2) = LamApp (eval1cbn e1) e2
eval1cbn (LamVar x) = (LamVar x)

--modified code to evaluate from right hand side instead of left
eval2cbn :: LamExpr -> LamExpr
eval2cbn (LamAbs x e) = (LamAbs x e)
eval2cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval2cbn (LamApp e1 e2) = LamApp e1 (eval2cbn e2)
eval2cbn (LamVar x) = (LamVar x)