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

plusArg= (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))


varArit :: Parser Arit
varArit = do
  i <- nat
  return (Var i)


addArit :: Parser Arit
addArit = do e1 <-  allArit
             symbol "+"
             e2 <-  lowerArit
             return (Add e1 e2)

secArit :: Parser Arit
secArit = do symbol "(+"
             e1 <-lowerArit
             symbol ")"
             return (Section e1)

-- inOrder :: [Arit] -> Arit
-- inOrder (x:[])=x
-- inOrder (x:y:xs)= foldl f (App x y) xs
--            where
--              f m n= App m n  

-- allArit :: Parser Arit
-- allArit = do val<- varArit
--                     return val
--                <|> do symbol "("
--                       sec <- lowerArit
--                       symbol ")"
--                       return sec
--                <|> do section <- token secArit
--                       value <- token lowerArit
--                       return (App section value)


firstArit :: Parser Arit
firstArit = do val<- varArit
               return val
 
secondArit:: Parser Arit
secondArit = do symbol "("
                sec <- lowerArit
                symbol ")"
                return sec

thirdArit:: Parser Arit
thirdArit= do section <- secArit
              value <- lowerArit
              return (App section value)

highArit :: Parser Arit
highArit = lowerArit <|> secArit <|> downArit
lowerArit = addArit <|> allArit
allArit = firstArit <|> secondArit <|> thirdArit
downArit =  varArit <|> empty

transformeStart :: Arit -> LamExpr
transformeStart x@(Var z)=(LamAbs 1 (LamAbs 2 (transforme z)))
transformeStart s@(Section y@(Var z))=(LamApp (transformeStart (Var z)) (plusArg))
transformeStart (Section e@(Add e1@(Var d) e2@(Var c))) = (LamApp (transformeStart e) (plusArg))
transformeStart (Section a@(App e1 e2)) = (LamApp (transformeStart a) (plusArg))
transformeStart a@(Add e1@(Var d) e2@(Var c)) = (LamApp (transformeStart (Section (Var c))) (transformeStart (Var (d))))
transformeStart (App e1 e2)=(LamApp (transformeStart e1) (transformeStart e2))
--transformeStart a@(Add e1 e2) = 

transforme :: Int -> LamExpr
transforme  0 = (LamVar 2)
transforme  x = (LamApp (LamVar 1) (transforme (x-1)))

compileArith :: String -> Maybe LamExpr
compileArith s =  case (parse highArit s) of
  [(n, [])] -> Just (transformeStart(n))
  _ -> Nothing
