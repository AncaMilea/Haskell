-- Define a function longestCommonSubList :: Eq a => [[a]] -> [a] that returns the longest sub-list of each of the
-- finite list of finite lists supplied to the function. For example, longestCommonSubList [[1,2,3], [0,1,3], [1,3,4]] =
-- [1,3]. Note that for this exercise, [1,3] is defined to be a sub-List of [1,2,3], so that elements of the sub-list do
-- not need to occur in neighbouring positions of the super-list. More precisely s is a sub-list of t if and only if s
-- can result from deleting zero or more members of t. Your implementation should satisfy the equation
-- longestCommonSubList [] = []. This is valid because any list is a sub-list of all members of an empty list. 

miniSubList :: [a] -> [[a]]
miniSubList [] = [[]]
miniSubList (x:xs) = [x:sublist | sublist <- miniSubList xs] ++ miniSubList xs

noEmpty :: [a] -> [[a]]
noEmpty xs = filter (not. null) (miniSubList xs) 

subListoflist :: (Eq a) => [[a]] -> [[[a]]]
subListoflist []= [[]]
subListoflist (x:xs) = sorted : subListoflist xs
                    where
                        sorted= listDesc (length (head noDup)) noDup
                        noDup= noDuplicateElem list 
                        list= noEmpty x

noDuplicateElem :: (Eq a) => [[a]] -> [[a]]
noDuplicateElem [[]] = [[]]
noDuplicateElem [x] = [x]
noDuplicateElem (x:xs) = x : [ k  | k <- noDuplicateElem(xs), k /=x ]

lengthDesc :: Eq a =>Int-> [[a]] ->[[a]]
lengthDesc n ys = [y|y<-ys, length y == n]


listDesc :: Eq a => Int ->[[a]] ->[[a]]
listDesc 1 xs = lengthDesc 1 xs
listDesc n xs = (lengthDesc n xs) ++ listDesc (n-1) xs 

count :: Eq a =>[a] -> [[[a]]] ->Int
count x yss = length [y|ys<-yss, y<-ys, y==x]

countListinLists :: Eq a=>[[a]]->[[[a]]]->[a]
countListinLists [[]] xss = []
countListinLists (x:xs) xss
                    |((length xss) == sumi) = x
                    | otherwise = countListinLists xs xss
                where
                    sumi= count x xss

countValid:: Eq a =>[[[a]]] ->[a]
countValid xss = countListinLists (head(xss)) xss

longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList ns | (length ns ==1) = head ns
                        | otherwise = countValid lista
                where lista = filter (not. null) (subListoflist ns)