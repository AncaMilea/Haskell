zipL :: ([Int],[Int])->[[Int]]
zipL ([],[])=[]
zipL (xs,ys)
          | length xs == length ys = [head xs, head ys]:zipL (drop 1 xs, drop 1 ys)
          | otherwise = error "not equal"

unzipL :: [[Int]]->([Int],[Int])
unzipL []=([],[])
unzipL ([x,y]:xss)=(x:xs,y:ys)
         where (xs,ys)=unzipL xss

