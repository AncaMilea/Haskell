{-Challenge	2:	Pretty	Printing	a	Let	Expression
You	 are asked	 to	 write	 a	 “pretty	 printer”	 to	 display	 a	simple	 let	 expression.	 	 Your	 output	 should	
produce	a	syntactically	correct	let	expression.		In	addition,	your solution	should	omit	brackets	where	
these	are	not	 required.		That	is	 to	say,	you can	omit	brackets	when	the	resulting	string	 represents	
the	same	abstract	expression	as	the	original	one.		Beyond	that	you	are	free	to	format	and	lay	out	the	
expression	as	you	choose	in	order	to	make	it	shorter	or	easier	to	read or	both.		

Some	examples	of	pretty	printing	are:
Let	[1]	(Var	2)	(Var	1) "let	x1	=	x2	in	x1"
Let	[1,2]	(Var	2)	(App	(Var	3)	(Var	1)) "let	x1	x2	=	x2	in	x3 x1"
App	(Let	[1,2]	(Var	2)	(Var	3))	(Var	1) "(let	x1	x2	=	x2	in	x3) x1"
App	(Var	1)	(App	(Var	2)	(Var	3)) “x1	(x2	x3)”
App	(App	(Var	1)	(Var	2))	(Var	3) “x1	x2	x3”-}

import Data.Char
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
-- replace the definition below with your solution
prettyPrint :: Expr -> String
prettyPrint (Let xs e1 e2) = "let "++(getVar xs)++" = "++(prettyPrint e1)++" in "++(prettyPrint e2)
prettyPrint (App e1@(Var x) e2@(Var y))= (prettyPrint (Var x))++" "++(prettyPrint (Var y))
prettyPrint (App e1@(Var x) e2@(Let xs e3 e4))= (prettyPrint (Var x))++" "++(prettyPrint (Let xs e3 e4))
prettyPrint (App e1@(Let xs e3 e4) e2@(Var x))= "("++(prettyPrint (Let xs e3 e4))++") "++(prettyPrint (Var x))
prettyPrint (App e1@(Let xs e3 e4) e2@(Let ys e5 e6))= "("++(prettyPrint (Let xs e3 e4))++") "++(prettyPrint (Let ys e3 e4))
prettyPrint (App e1@(App e3 e4) e2@(Var x))= (prettyPrint (App e3 e4))++" "++(prettyPrint (Var x))
prettyPrint (App e1@(Var x) e2@(App e3 e4))= (prettyPrint (Var x))++" ("++(prettyPrint (App e3 e4))++")"
prettyPrint (App e1@(App e3 e4) e2@(Let xs e5 e6))= (prettyPrint (App e3 e4))++" "++(prettyPrint (Let xs e5 e6))
prettyPrint (App e1@(Let xs e5 e6) e2@(App e3 e4))= "("++(prettyPrint (Let xs e5 e6))++") ("++(prettyPrint (App e3 e4))++")"
prettyPrint (App e1@(App e3 e4) e2@(App e5 e6))=(prettyPrint (App e3 e4))++" ("++(prettyPrint (App e5 e6))++")"
prettyPrint (Var e1)= "x"++show(e1) 

getVar :: [Int] ->String
getVar []=""
getVar [s]= prettyPrint (Var s)
getVar (s:ss)= "x"++show(s)++" "++getVar ss
