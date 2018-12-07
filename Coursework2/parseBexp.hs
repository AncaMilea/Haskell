-- Example for lecture to demonstrate simple monadic parsing
import Parsing

data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)

-- Parsing Code
varExpression :: Parser Expr
varExpression = do
  symbol "x"
  i <- nat
  return (Var i)


letVariables :: Parser [Int]
letVariables = do
  symbol "x"
  x <- nat
  xs <- many (do 
    symbol "x"
    nat)
  return (x:xs)


letExpression :: Parser Expr
letExpression = do symbol "let"
                   xs <- letVariables
                   symbol "=" 
                   e1 <- allExpressions
                   symbol "in"
                   e2 <- allExpressions
                   return (Let xs e1 e2)

appExpression :: Parser Expr
appExpression = do symbol "("
                   e1 <- allExpressions
                   symbol ")"
                   return e1

inOrder :: [Expr] -> Expr
inOrder (x:[])=x
inOrder (x:y:xs)= foldl f (App x y) xs
           where
             f m n= App m n  

allExpressions :: Parser Expr
allExpressions = do 
                 xs<-many(higherExpr)
                 return (inOrder xs)        

-- Define the top level entry point
-- use choice <|> to write alternatives in the grammar
higherExpr :: Parser Expr
higherExpr = appExpression <|> lowerExpr 
lowerExpr = letExpression <|> bottomExpr
bottomExpr = varExpression <|> empty

parseLet :: String -> Maybe Expr
parseLet s = case (parse allExpressions s) of
  [(n, [])] -> Just n
  _ -> Nothing



