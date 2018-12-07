-- Challenge 1:  Converting  a Let Expression  to  Lambda  Calculus
-- The  first   challenge   requires  you   to  convert   a   let   expression  into  an  equivalent  lambda  calculus 
-- expression   using,  for   example,  the   translation   given   below.    The   lambda  expression  will  be 
-- represented using the following data  type:
--               data  LamExpr = LamApp  LamExpr LamExpr   |   LamAbs Int  LamExpr   |   LamVar  Int   
--                                 deriving  (Show,  Eq)
-- which represent an  application,  a lambda  abstraction,  and a variable  such  as  x1 respectively.
-- Conversion  constructs a  lambda  abstraction and applies this  as  described below.
-- First consider  a simple case let x1  = e in  d,  where e and d are also  let expressions.  Here  the required  
-- result   is  d’ but  with  any   free  occurrence  of  x1 replaced   by  e’,   where   d’  is  the lambda  expression 
-- resulting  from  converting  d,  and   e’ is   the   lambda  expression  resulting   from  converting  e.    This 
-- substitution  is  expressed in  lambda  calculus  using the abstraction and application (λx1 -> d’) e’.   
-- Now consider  a more  complex let expression  such  as  let x1  x2  = e in  d.    Here  x1  is  a  function with  
-- parameter x2 whose   body  is  the   let   expression  e (or   after   conversion,  the  lambda  expression  e’).   
-- Hence the required  result  is  the lambda  expression  d’ but  with  x1 replaced by  the abstraction λx2 ->  
-- e’.   The required  lambda  expression  is  therefore (λx1 -> d’) (λx2  ->  e’).    And so  on  for let expressions 
-- with  2,  3 or  more  arguments.
-- (Note that  there are other let conversion  rules,  which give  different results in  some cases2 so  be  sure  
-- to  implement  the  version given here.   This  version is  based on  Dana  Scott’s  simple let expression.
-- Specifically, for let x1  = e in  d, then variable  x1 is bound in  d but is  not bound in  e.
-- Here  are some  examples  of  let conversions:
-- let x1  = x1  in  x2   (λx1 ->  x2) x1
-- let x1  x2  = x2  in  x1 (λx1 ->  x1) (λx2  ->  x2)
-- let x1  x2  x3  = x3  x2  in  x1  x4 (λx1 ->  x1  x4) (λx2  ->  λx3 ->  x3  x2)
-- let x1  = x2  in  let x3  = x4  in  x1  x3 (λx1 ->  (λx3  ->  x1  x3) x4) x2
-- Challenge 1
--convertLet (Let [1] (Var 1) (Var 2)) `equivLam` LamApp (LamAbs 1 (LamVar 2)) (LamVar 1)
--convertLet (Let [1,2] (Var 2) (Var 1)) `equivLam` LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))
--convertLet (Let [1,2,3] (App (Var 3) (Var 2)) (App (Var 1) (Var 4))) `equivLam` LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamVar 3) (LamVar 2))))
--convertLet (Let [1] (Var 2) (Let [3] (Var 4) (App (Var 1) (Var 3)))) equivLam` LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))) (LamVar 2)
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression -- replace the definition below with your solution
convertLet :: Expr -> LamExpr
convertLet (Let xs e1 e2) = LamApp (convertAbs (head(xs)) e2) (convertLetRem (drop 1 xs) e1)
convertLet (App e1 e2) = (LamApp(convertLet e1) (convertLet e2))
convertLet (Var x)= LamVar x


convertAbs :: Int -> Expr ->LamExpr
convertAbs xs e1 = LamAbs xs (convertLet e1)

convertLetRem :: [Int] -> Expr ->LamExpr
convertLetRem [] e1= (convertLet e1)
convertLetRem (x:xs) e1 = LamAbs x (convertLetRem xs e1)

convertApp :: [Int] -> Expr ->LamExpr
convertApp [] (App e1 e2)= (LamApp(convertLet e1) (convertLet e2))
convertApp [x] (App e1 e2)= (LamApp (convertAbs x e1) (convertLet e2))
convertApp (x:xs) (App e1 e2)= (LamApp (convertAbs x e2) (convertLetRem xs e1)) 