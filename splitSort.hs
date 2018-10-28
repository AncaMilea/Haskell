-- Exercise 1
-- Define a function splitSort :: Ord a => [a] -> [[a]] that splits the given list into non-empty sub-lists each of which
-- is either in strictly ascending order, in strictly descending order, or contains all equal elements. For example,
-- splitSort[1,2,3,2,1,1,1] = [[1,2,3],[2,1],[1,1]]. Note that your implementation should be greedy, meaning that it
-- consumes as many elements as possible to use in the current sub-list before starting a new one. 

splitSort :: Ord a => [a] -> [[a]]
splitSort ns
            | length ns ==0 =[]
            | length ns <3 =[ns]
            | otherwise = splitADE (take 2 ns) (drop 2 ns)

splitADE :: Ord a =>[a]->[a]->[[a]]
splitADE xs (n:[])  
               | (length xs ==1) && ((last xs < n)|| (last xs > n) || (last xs == n)) = (xs++[n]) :[]
               | ((last xs < n && head xs < head(tail xs) ) || (last xs > n && head xs > head(tail xs))) = (xs++[n]) : []
               | ((last xs < n && head xs >= head(tail xs) ) || (last xs > n && head xs <= head(tail xs))) = xs : [n] : [] 
               | ((last xs == n && head xs == head(tail xs))) = (xs++[n]) : []
               | ((last xs == n && not (head xs == head(tail xs)))) = xs:[n] : []
splitADE xs (n:ns) 
               | (length xs ==1) && ((last xs < n)|| (last xs > n)) = splitADE (xs++[n]) (ns)
               | (length xs ==1) && (last xs == n) = splitADE (xs++[n]) (ns)
               | ((last xs < n && head xs < head(tail xs) ) || (last xs > n && head xs > head(tail xs))) = splitADE (xs++[n]) (ns)
               | ((last xs < n && head xs >= head(tail xs) ) || (last xs > n && head xs <= head(tail xs))) = xs: splitADE ([n]) (ns)
               | ((last xs == n && head xs == head(tail xs))) = splitADE (xs++[n]) (ns)
               | ((last xs == n && not (head xs == head(tail xs)))) = xs: splitADE ([n]) (ns)
               
