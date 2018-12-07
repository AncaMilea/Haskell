-- Exercise 14
-- The square shell pairing function was defined by Rosenberg and Strong in 1972, and analysed by Szudzik (2017)2. This pairing function is illustrated by the table below. 
-- It takes two natural numbers as arguments and produces a single natural number as its result (thereby proving that ℕ×ℕ is the same size as ℕ, and hence countable). 
-- For example, the value shown at position (3, 2) is 13 which is in the fourth and final square shell  illustrated here, so that Pair 3 2 = 13.

-- You are asked to define a function unPairAndApply the supplied natural number n to a pair (x,y) by inverting the pairing function described above, and then returns 
-- the result of applying the function f provided as its second parameter to the two values resulting from unpairing n. Hence unPairAndApply 13 (–) = 1 since 3 – 2 = 1.
 -- Note that the function f is supplied in curried form, so that it should be applied using f x y rather than f(x,y).

unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = (f x) y
             where 
                x= fst (ns)
                y= snd (ns)
                ns= getValues n

getValues :: Int ->(Int, Int)
getValues z 
            | (z - m^2) < m = (z - m^2,m)
            | otherwise = (m, m^2+2*m-z)
           where 
              m= floor(sqrt(fromIntegral(z)))