-- Exercise 13
-- Define a function differentStream, that returns a stream of binary digits which is not present in the stream of
-- streams supplied to the function. For example, differentStream [[1..], [2..], [3..], ..] = [0..] would be a valid
-- result. You may assume that the actual parameter passed to this function is an infinite stream of infinite
-- streams of integers. Note that the existence of this function shows that, for example, ℕ → ℕ is uncountable.

differentStream :: [[Int]] -> [Int]
differentStream [] = []
differentStream ls = d ls 0

d:: [[Int]]->Int->[Int]
d [] pos = []
d (l:ls) pos
           | nr>0 =0:d ls (pos+1)
           | nr ==0 =1:d ls (pos+1)
           where
           nr = ((l !! pos))
