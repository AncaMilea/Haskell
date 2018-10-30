-- Exercise 13
-- Define a function differentStream, that returns a stream of binary digits which is not present in the stream of
-- streams supplied to the function. For example, differentStream [[1..], [2..], [3..], ..] = [0..] would be a valid
-- result. You may assume that the actual parameter passed to this function is an infinite stream of infinite
-- streams of integers. Note that the existence of this function shows that, for example, ℕ → ℕ is uncountable.

differentStream :: [[Int]] -> [Int]
differentStream [] = []
differentStream (l:ls) = split id [l] ls

split :: ([[a]] -> [[a]]) -> [[a]] -> [[a]] -> [a]
split a [] ls      = case ls of
                          []     -> split id [] (a [])
                          (l:ls) -> split id (a [l]) ls
split a (l : ls) r = case l of
                          []     -> split a ls r
                          (x:xs) -> x : split (a . (xs :)) ls r

isDifferentStream :: [[Int]] -> [Int] -> Int -> Bool
isDifferentStream nns ns n = 
    let ns' = take n ns
        nns' = map (take n) (take n nns) 
    in length ns' == 10 && 
       all (\xs -> length xs == 10) nns' &&
       not (ns' `elem` nns')

testStream1 :: [[Int]]
testStream1 = [0..]:testStream1

testStream2 :: [[Int]]
testStream2 = repeatStream 0
repeatStream :: Int -> [[Int]]
repeatStream x = [x..]:(repeatStream (x+1))
