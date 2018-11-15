-- Exercise 6
-- Use your solution to the previous exercise to find the nearest root to the given x of a polynomial f(x). Note
-- that f^2 is always zero or positive, so that any root of any function f is a minimum of f^2, and vice versa. The
-- nearestRoot function is supplied a list of coefficients defining the polynomial, so that nearestRoot [-18.0, 0.0,
-- 2.0] 0.0 5.0 1e-5 ~= 3.0 since the quadratic 2x^2 – 18 has 3 as its only positive root. Note that the coefficients are
-- supplied in the reverse of the usual order, with the constant value at the head and the coefficient associated
-- with the highest power of x as the last value in the list. The second, third and fourth parameters to this
-- function are the same as those for the hillClimb function in the previous exercise.
rad1 = ((sqrt(5)-1)/2) 
rad2 = ((3-sqrt(5))/2)

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = gssrec d x x' eps (x' - x)

gssrec :: (Float->Float) ->Float -> Float -> Float -> Float -> Float 
gssrec f a b eps h  
                 | h1 <= (sqrt eps) = (new_a+new_b) / 2
                 | fc1 >fd1 = gssrec f new_a d1 eps (h1*rad1)
                 | otherwise = gssrec f c1 new_b eps (h1*rad1)
              where
                  new_a = min a b
                  new_b = max a b 
                  h1 = abs(new_b-new_a)
                  c1 = (new_a+(rad2*h1))
                  d1 = (new_a+(rad1*h1))
                  fc1 = f c1
                  fd1 = f d1 

nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb equat x x' eps
                 where
                    equat= poly xs

powersX :: Num a => a -> [a]
powersX x = map (x^) [0..]

poly :: Num a => [a] -> a -> a
poly p = \x -> -(sum (getTogether (*) p (powersX x)))^2

getTogether :: (a->b->c)->[a]->[b]->[c]
getTogether f [] _ = []
getTogether f _ [] = []
getTogether f (x:xs) (y:ys) = (f x) y : getTogether f xs ys