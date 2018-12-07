import Parsing
data ExtExpr = ExtApp ExtExpr ExtExpr | ExtLam [Int] ExtExpr | ExtVar Int
	deriving (Show, Eq)

var :: Parser ExtExpr
var = do
	char 'x'
	i <- nat
	return (ExtVar i)

vars :: Parser [Int]
vars = do
	char 'x'
	x <- nat
	xs <- many (do
		char 'x'
		nat)
	return (x:xs)

expr :: Parser ExtExpr
expr = do
	  	xs <- many (dummy)
	  	if length xs == 1 
			then return $ head xs
			else return $ mkTree xs
	where
		mkTree (x:y:xs) = foldl f (ExtApp x y) xs
		f y x = ExtApp y x

dummy :: Parser ExtExpr
dummy = do
	  	symbol "("
	  	y <- expr
	  	symbol ")"
	  	return y
	  <|> do
	  	x <- var
	  	return x
	  <|> do
	  	symbol "\\"
	  	y <- vars
	  	symbol "->"
	  	z <- expr
	  	return (ExtLam y z)
	  <|> empty


parseLam :: String -> Maybe ExtExpr
parseLam xs = case (parse expr xs) of
	[(n, [])] -> Just n
	_ -> Nothing