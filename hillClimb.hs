-- Exercise 5
-- Implement a function hillClimb to return an approximation of the local maximum of a given function f within
-- the interval from x0 to x1, values which are supplied as its second and third parameters. Your solution
-- should aim to return a value within ε of the true local maximum, where ε is supplied as the fourth parameter.
-- It should use the golden section search method as explained for example on Wikipedia1
-- to reduce the search interval efficiently, converging when the width of this interval is less than or equal to √ε. 

rad1 = ((sqrt(5)-1)/2) 
rad2 = ((3-sqrt(5))/2)

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = gssrec d x x' eps (x' - x) 0 0 0 0 

gssrec :: (Float->Float) ->Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float ->Float
gssrec f a b eps h c d fc fd 
                           | h1 <= (sqrt eps) = (new_a+new_b) / 2
                           | fc1 >fd1 = gssrec f new_a d1 eps (h1*rad1) 0 c1 0 fc1
                           | otherwise = gssrec f c1 new_b eps (h1*rad1) d1 0 fd1 0
                    where
                        new_a = min a b
                        new_b = max a b 
                        h1 = abs(new_b-new_a)
                        c1 = (new_a+(rad2*h1))
                        d1 = (new_a+(rad1*h1))
                        fc1 = f c1
                        fd1 = f d1 




                       

