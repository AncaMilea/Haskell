-- Exercise 15
-- The pairing function from the previous exercise can also be used to represent a binary tree as a single natural
-- number. For example, given the recursively defined type data Tree = Empty | Node Int Tree Tree deriving Show
-- the Empty tree can be represented by the number 0, and the tree Node n t u by the number Pair n (Pair t1 u1)
-- where the number t1 represents the tree t and the number u1 represents the tree u. For this exercise, you are
-- asked to implement a function isShellTreeSum which tests whether its supplied argument n is the result of
-- pairing x and y where x is the number representing a tree t and y is the result of summing the values at each
-- node of t. For example, the integer 3 represents the tree Node 1 Empty (Node 0 Empty Empty) which sums to 1.
-- Moreover the entry at position (3, 1) in the table above is 14, and hence isShellTreeSum 14 is true.
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


doUnPair :: Int ->(Int, Int)
doUnPair n = getValues n

summing :: (Int, Int) -> Int
summing (x,y)
        | (y == 0 || y== 1 || y==2) = x
        | otherwise = x+ summing (doUnPair y)

isShellTreeSum :: Int -> Bool
isShellTreeSum n
       | (summing tree) == sn = True
       | otherwise = False
       where
       	findNo = doUnPair n
       	fs = fst(findNo)
       	sn= snd(findNo)
       	tree = doUnPair fs
