-- Example for lecture to demonstrate simple monadic parsing
import Parsing

data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)

check :: Maybe a -> a
check Nothing = error ("No binding found for variable ")
check (Just b) = b

-- evaluation function to provide semantics to formulas
-- eval :: Substitution -> Expr -> Bool
-- eval _ Tru = True
-- eval _ Fls = False
-- eval s (Var p) = check $ lookup p s
-- eval s (And e1 e2) = (eval s e1) && (eval s e2)
-- eval s (Or e1 e2)  = (eval s e1) || (eval s e2)

-- Parsing Code

varsExp :: Parser [Int]
varsExp = do
  symbol "x"
  x <- nat
  xs <- many (do 
    symbol "x"
    nat)
  return (x:xs)
-- Expressions
-- Define a parser for each kind of expression
-- We use "ident" and "symbol" from Parsing.hs
varExp :: Parser Expr
varExp = do
  symbol "x"
  i <- nat
  return (Var i)

intExp :: Parser Int
intExp = do s <- varExp
            let i = retInt(s)
            return (i)

retInt (Var x) =x

letExp :: Parser Expr
letExp = do symbol "let"
            xs <- varsExp
            symbol "=" 
            e1 <- doAll
            symbol "in"
            e2 <- doAll
            return (Let xs e1 e2)

appExp :: Parser Expr
appExp = do symbol "("
            e1 <- doAll
            symbol ")"
            return e1

doAll :: Parser Expr
doAll = do 
           xs<-many(expr)
           return (recur xs)
        where
           recur (x:[])=x
           recur (x:y:xs)= foldl f (App x y) xs
           f x y= App x y            

-- Define the top level entry point
-- use choice <|> to write alternatives in the grammar
expr :: Parser Expr
expr = appExp <|> lowerExpr 
lowerExpr = letExp <|> evenLowerExpr
evenLowerExpr = varExp <|> empty
-- expr :: Parser Expr
-- expr = appExp 
-- lowerExpr = appExp <|> varExp <|> letExp

-- Finally, define the actual function to parse input strings
-- NB, this definition is just for demonstration. It is not a good
-- definition and needs improving
parseLet :: String -> Maybe Expr
parseLet s =  case (parse doAll s) of
  [(n, [])] -> Just n
  _ -> Nothing



