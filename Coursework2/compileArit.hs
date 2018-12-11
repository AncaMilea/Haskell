-- Challenge	5:	Compiling	Arithmetic	Expressions	to	Lambda	Calculus
-- As	 noted	above,	 the	 lambda	 calculus	 can	encode	arithmetic.	 	 For	 this	 challenge, you	are	asked	 to	
-- write	 a	 function	 which	 translates	 or	 compiles	 an	 expression	 of	 natural	 number	 arithmetic	 to	 an	
-- equivalent	lambda	calculus	expression.		For	example,	it	should	compile	“0”	 to	 the	lambda	calculus	
-- expression	that	encodes	zero,	“1”	to	the	lambda	calculus	expression	that	encodes	one,	and	“(+	1)”	to	
-- the	lambda	calculus	expression	that	encodes	the	successor	function.		
-- The	concrete	syntax	of	these	expressions	is	given	in	BNF	as:
-- 								ArithmeticExpression ::=	Value |	Section
-- 								Section ::= “(“	“+”	Value “)”		
-- 								Value	::=	Section Value	| Natural | Value	“+” Value	|	(	Value	)			
-- 								Natural	::=	Digit |	Natural	Digit
-- 								Digit	::=	“0”	|	“1”	|	…	|	“9”
-- The	same	operator	precedence	and	associativity	applies	as	in	Haskell.		You	may	use	monadic	parsing,	
-- recursive	descent	parsing,	or	any	other	means	of	parsing	the	input	string.		You	are	recommended	to	
-- parse	this string into	some	 abstract	 data	 type,	 or	 types, before	then	translating	 this	 to	 a	 lambda	
-- calculus	expression.		(Although	it	is	possible	to	parse	and	compile	at	the	same	time,	this	approach	is	
-- usually	more	difficult	to	implement	and	maintain.)
-- It	 is	required	the	 lambda	 expressions	 your	 compiler	 generates	 will	 conform	 to	 the	 usual	 rules	 of	
-- arithmetic,	 but	 using	 beta	 equivalence	 as	 the	 “equality”	 relation.	 	 For	 example, compiling	 “(+	 1)”	
-- should	 result	 in	 a	 lambda	expression	 which	 when	 applied	 to	 the	 result	 of	 compiling	 “2”	 returns	 a	
-- lambda	expression	which	is	beta	equivalent	 to	 the	result	of	compiling	“3”.	 You	are	advised	 to	use	
-- the	Church	encoding described	in	the	Wikipedia	article	cited above,	and	indicated	in	the	table	below,	
-- although	others	exist.		
-- Some	possible	examples	are	as	follows.	Note	that your	solutions	may	differ	from	these	depending	on	
-- your	chosen	encoding	but	they	ought	to	be	beta-equivalent.
-- "0++" Nothing																								--		syntactically	invalid
-- "0" Just	(λx	->	(λy	->	y))
-- "1" Just	(λx	->	(λy	->	x	y))
-- "2" Just	(λx	->	(λy	->	x	x	y))
-- "(+1)" Just	((λx	->	(λy	->	x	y))	(λx	->	λy	->	λz ->	y	(x	y z)))
-- The	final	result can	be	simplified,	although	you	are	not	required	to	implement	this	simplification.
import Data.Char
import Parsing

data Arit = App Arit Arit| Section Arit | Add Arit Arit | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

varExpression :: Parser Arit
varExpression = do
  i <- nat
  return (Var i)


addExpression :: Parser Arit
addExpression = do e1 <- token allExpressions
                   symbol "+"
                   e2 <- token evenlower
                   return (Add e1 e2)

secArit :: Parser Arit
secArit = do symbol "(+"
             e1 <-lowerExpr
             symbol ")"
             return (Section e1)

-- inOrder :: [Arit] -> Arit
-- inOrder (x:[])=x
-- inOrder (x:y:xs)= foldl f (App x y) xs
--            where
--              f m n= App m n  

allExpressions :: Parser Arit
allExpressions = do val<- token varExpression
                    return val
               <|> do symbol "("
                      sec <- token evenlower
                      symbol ")"
                      return sec
               <|> do section <- token secArit
                      value <- token evenlower
                      return (App section value)

lowerExpr :: Parser Arit
lowerExpr = evenlower <|> secArit <|> bottomExpr
evenlower = addExpression <|> allExpressions
bottomExpr =  varExpression <|> empty

compil::String ->Maybe Arit
compil s =  case (parse lowerExpr s) of
  [(n, [])] -> Just n
  _ -> Nothing

transformeStart :: Arit -> LamExpr
transformeStart x@(Var z)=(LamAbs 1 (LamAbs 2 (transforme z)))
transformeStart s@(Section y@(Var z))=(LamApp (transformeStart (Var z)) (transformeSec (s)))
transformeStart a@(Add e1@(Var d) e2@(Var c)) = (LamApp (transformeStart (Var (d+c) )) (transformeSec (Section (Var (d+c)))))
--transformeStart a@(Add e1 e2) = 

transformeSec :: Arit -> LamExpr
transformeSec (Section x@(Var z)) = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (transforme (z)) (LamVar 3))))))

transforme :: Int -> LamExpr
transforme  0 = (LamVar 2)
transforme  x = (LamApp (LamVar 1) (transforme (x-1)))

compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s = Nothing

--lamAbF :: Int ->Int -> LamExpr
-- lamAbF x y
--           |x /=y = (LamAbs x (lamAbF (x+1) y) )
--           |x ==y = (LamAbs x)
--succChurch = \n -> \f -> \x -> f (n f x)
