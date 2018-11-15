-- Exercise 10
-- In this exercise, a rectangle is represented by a pair of integer valued coordinates identifying its bottom-left
-- and top-right corners in that order. If the top-right corner is below or to the left of the second corner, the
-- rectangle is empty. These rectangles all have horizontal and vertical edges. You are given a list of such
-- rectangles representing a black and white image constructed using these rectangles. Each rectangle’s corners
-- and edges are considered to be part of the image, as well as all points with integer valued coordinates in each
-- rectangle’s interior. If the rectangles overlap, there may be a shorter list that represents the same image. For
-- example, [Rectangle (0,0) (2,2), Rectangle (0,0) (1,1)] can be simplified to [Rectangle (0,0) (2,2)] since the second
-- rectangle is wholly contained within the first one. More complex scenarios are possible. Write a function
-- simplifyRectangleList that gives a minimal sequence which represents the same image as the originally
-- supplied rectangle list. You do not need to implement an optimal solution, as this requires sophisticated data
-- structures, but your solution is expected to have polynomial time complexity rather than exponential.

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)


noDuplicateElem []= []
noDuplicateElem (x:xs) = x : [ k  | k <- noDuplicateElem(xs), k /=x ]


eliminateRect x xs = [z|z<-xs, z/=x]

eliminateRect2::[Rectangle]->[Rectangle]->[Rectangle]
eliminateRect2 [] ys =ys
eliminateRect2 (x:xs) ys = eliminateRect2 xs (eliminateRect x ys)


invalidRectangle :: [Rectangle] -> [Rectangle]
invalidRectangle xs = [Rectangle (a,b) (c,d)| Rectangle (a,b) (c,d) <-xs, a<=c, b<=d]

checkFUllage [] ys =ys
checkFUllage ((Rectangle (a,b) (c,d)):xs) ys
                  | length new_xs ==0 = checkFUllage xs ys
                  | (length new_xs) > 0 = checkFUllage xs elemi
                  where
                    new_xs= fullRectangleSwitch (Rectangle (a,b) (c,d)) elemD
                    elemi= eliminateRect2 new_xs (ys)
                    elemD= eliminateRect (Rectangle (a,b) (c,d)) ys
                    
checkPoint [] ys =ys
checkPoint ((Rectangle (a,b) (c,d)):xs) ys
                  | length new_xs ==0 = checkPoint xs ys
                  | (length new_xs) > 0 = checkPoint xs elemi
                  where
                    new_xs= pointIn (Rectangle (a,b) (c,d)) elemD
                    elemi= eliminateRect2 new_xs (ys)
                    elemD= eliminateRect (Rectangle (a,b) (c,d)) ys

pointIn ::Rectangle ->[Rectangle]->[Rectangle]
pointIn (Rectangle (a,b) (c,d)) [] = []
pointIn (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                                    | a==c, b==d, a>=m, d<=p = [Rectangle (a,b) (c,d)] ++ pointIn (Rectangle (a,b) (c,d)) xs
                                    | m==o, n==p, m>=a, p<=d = [Rectangle (m,n) (o,p)] ++ pointIn (Rectangle (a,b) (c,d)) xs
                                    | otherwise = pointIn (Rectangle (a,b) (c,d)) xs

fullRectangleSwitch ::Rectangle ->[Rectangle]->[Rectangle]
fullRectangleSwitch (Rectangle (a,b) (c,d)) [] = []
fullRectangleSwitch (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                                    | a<=m, b<=n, c>=o, d>=p = [Rectangle (m,n) (o,p)] ++ fullRectangleSwitch (Rectangle (a,b) (c,d)) xs
                                    | m<=a, n<=b, o>=c, p>=d = [Rectangle (a,b) (c,d)] ++ fullRectangleSwitch (Rectangle (a,b) (c,d)) xs
                                    | otherwise = fullRectangleSwitch (Rectangle (a,b) (c,d)) xs

nearReact ::Rectangle ->[Rectangle]->[Rectangle]
nearReact (Rectangle (a,b) (c,d)) [] = []
nearReact (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                               | m==c, d==p, b==n = [Rectangle (a,b) (o,p)] ++ nearReact (Rectangle (a,b) (c,d)) xs
                               | a==o, b==n, d==p = [Rectangle (m,n) (c,d)] ++ nearReact (Rectangle (a,b) (c,d)) xs
                               | otherwise =nearReact (Rectangle (a,b) (c,d)) xs

nearReactEmpty ::Rectangle ->[Rectangle]->[Rectangle]
nearReactEmpty (Rectangle (a,b) (c,d)) [] = []
nearReactEmpty (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                               | m==c, d==p, b==n = [Rectangle (a,b) (c,d),Rectangle (m,n) (o,p)] ++ nearReact (Rectangle (a,b) (c,d)) xs
                               | a==o, b==n, d==p = [Rectangle (a,b) (c,d),Rectangle (m,n) (o,p)] ++ nearReact (Rectangle (a,b) (c,d)) xs
                               | otherwise =nearReact (Rectangle (a,b) (c,d)) xs

checkNear [] ys =ys
checkNear ((Rectangle (a,b) (c,d)):xs) ys
                  | length new_xs ==0 = checkFUllage xs ys
                  | (length new_xs) > 0 = checkFUllage new_list new_list
                  where
                    new_xs= nearReactEmpty (Rectangle (a,b) (c,d)) elemD
                    update_xs= nearReact (Rectangle (a,b) (c,d)) elemD
                    elemi= eliminateRect2 new_xs (ys)
                    elemD= eliminateRect (Rectangle (a,b) (c,d)) ys
                    new_list= update_xs ++ elemi

upReact ::Rectangle ->[Rectangle]->[Rectangle]
upReact (Rectangle (a,b) (c,d)) [] = []
upReact (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                               | m==a, d==n, c==o = [Rectangle (a,b) (o,p)] ++ upReact (Rectangle (a,b) (c,d)) xs
                               | a==m, b==p, o==c = [Rectangle (m,n) (c,d)] ++ upReact (Rectangle (a,b) (c,d)) xs
                               | otherwise =upReact (Rectangle (a,b) (c,d)) xs

upReactEmpty ::Rectangle ->[Rectangle]->[Rectangle]
upReactEmpty (Rectangle (a,b) (c,d)) [] = []
upReactEmpty (Rectangle (a,b) (c,d)) ((Rectangle (m,n) (o,p)):xs)
                               | m==a, d==n, c==o = [Rectangle (a,b) (c,d),Rectangle (m,n) (o,p)] ++ upReact (Rectangle (a,b) (c,d)) xs
                               | a==m, b==p, o==c = [Rectangle (a,b) (c,d),Rectangle (m,n) (o,p)] ++ upReact (Rectangle (a,b) (c,d)) xs
                               | otherwise =upReact (Rectangle (a,b) (c,d)) xs

checkUp [] ys =ys
checkUp ((Rectangle (a,b) (c,d)):xs) ys
                  | length new_xs ==0 = checkUp xs ys
                  | (length new_xs) > 0 = checkUp new_search new_list
                  where
                    new_xs= upReactEmpty (Rectangle (a,b) (c,d)) elemD
                    update_xs= upReact (Rectangle (a,b) (c,d)) elemD
                    elemi= eliminateRect2 new_xs (ys)
                    elemD= eliminateRect (Rectangle (a,b) (c,d)) ys
                    new_list= update_xs ++ elemi
                    new_search= update_xs ++ elemi

simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs
                 | length ds ==1 = ds
                 | otherwise  = bn
            where
                ds = invalidRectangle rs
                ps= checkPoint ds ds
                tn = checkFUllage ps ps
                ns= checkNear tn tn
                us= checkUp ns ns 
                bn = noDuplicateElem tn
                    

-- combineRec:: [Rectangle]->[[Rectangle]]
-- combineRec xs = [[x,y]|x<-xs,y<-xs, x/=y]


-- -- callNoSub [[]] = 
-- -- callNoSub (x:y:xss) = interectSub x y ++ callNoSub xss 

-- interectSub :: [Rectangle]->[Rectangle]->[[Rectangle]]
-- interectSub xs ys
--                 | ( (head xs) == (head ys) && (last xs) == (last ys) ) || ((head xs) == (last ys) && (last xs) == (head ys)) = [xs]
--                 | otherwise = [xs,ys]

-- concating :: [[Rectangle]]->[Rectangle]
-- concating [x] = x
-- concating (x:xs) = x++concating xs

-- intersectRec (Rectangle (a,b) (c,d)) (Rectangle (a1,b1) (c1,d1)) 
--                         | a<=a1, b<=b1, c>=c1, d>=d1 =[Rectangle (a,b) (c,d)]
--                         | a1<=a, b1<=b, c1>=c, d1>=d = [Rectangle (a1,b1) (c1,d1)]
--                         | a==a1, b<=b1, c==c1, d<=d1 = [Rectangle (a,b) (c1,d1)]
--                         | a==a1, b1<=b, c==c1, d1<=d = [Rectangle (a1,b1) (c,d)]
--                         | a<=a1, b==b1, c<=c1, d == d1 = [Rectangle (a,b) (c1,d1)]
--                         | a1<=a, b==b1, c1<=c, d ==d1 = [Rectangle (a1,b1) (c,d)]
--                         | a<=a1, b<=b1, c>c1, d<d1 = [Rectangle (a,b) (c,d), Rectangle (a1,d) (c1,d1)]
--                         | a<a1, b<b1, c>=c1, d1<=d = [Rectangle (a,b) (c,d), Rectangle (a1,d) (c1,d1)]
--                         | a1<=a, b1<=b, c1>c, d1<d = [Rectangle (a1,b1) (c1,d1), Rectangle (a,d1) (c,d)]
--                         | a1<a, b1<b, c1>=c, d<=d1 = [Rectangle (a1,b1) (c1,d1), Rectangle (a,d1) (c,d)]
--                         | a<=a1, b>=b1, c>c1, d>d1 = [Rectangle (a,b) (c,d), Rectangle (a1,b1) (a,d1)]
--                         | a<a1, b>b1, c>=c1, d>=d1 = [Rectangle (a,b) (c,d), Rectangle (a1,b1) (a,d1)]
--                         | a1<=a, b1>=b, c1>c, d1>d = [Rectangle (a1,b1) (c1,d1), Rectangle (a,b) (a1,d)]
--                         | a1<a, b1>b, c1>=c, d1>=d = [Rectangle (a1,b1) (c1,d1), Rectangle (a,b) (a1,d)]
--                         | a1>=a, b1>=b, c1>c, d1<d = [Rectangle (a,b) (c,d), Rectangle (a1,d) (c1,d1)]
--                         | a1<a, b1>b, c1>a, c1<c, d1<d = [Rectangle (a,b) (c,d), Rectangle (a1,b1) (a,d1)]
--                         | a1<a, b1>b, c>c1, d1<d = [Rectangle (a,b) (c,d), Rectangle (a1,b1) (c1,d1)]
--                      -- | a<=m, b<=n, o<=c, p<=d = [Rectangle (a,b) (c,d)]
--                      -- | m<=a, n<=b, c<=o, d<=p = [Rectangle (m,n) (o,p)]
--                      -- | (n==b && a<=o && p==d && o<c) || (a==m && b<=p && o==c && p<d) = [Rectangle (m,n) (c,d)]
--                      -- | (m<=c && b==n && d==p && c<o) || (a==m && n<=d && c==o && d<p) = [Rectangle (a,b) (o,p)]
--                      -- | a<=m, o<=c,d<p,b==n = [Rectangle (a,b) (c,d), Rectangle (m,d) (o,p)]
--                      -- | m<=a, c<=o,p<d,b==n = [Rectangle (m,n) (o,p), Rectangle (a,p) (c,d)]
--                      -- | (p==d && n<b && c==o && a<m) || (p==d && n<b && a==m && o<c)  || (a<=m && b<n && o<=c &&  b<p && p<d) = [Rectangle (a,b) (c,d) , Rectangle (m,n) (o,b)]
--                      -- | (a==m && b==n && o<c && d<p) || (a<m && b==n && c==o && d<p) = [Rectangle (a,b) (c,d) , Rectangle (m,d) (o,p)]
--                      -- | (a>o && a<m && p<=d && b<=n && m<c) || (m>a && n==b && m<c && d>p && o>c) = [Rectangle (a,b) (c,d) , Rectangle (c,n) (o,p)]
--                      -- | m<a, a<o, b>=n, d<=p, o<c = [Rectangle (m,n) (o,p) , Rectangle (o,b) (c,d)]
--                      -- | (b<=n && p<=d && m<a && o<c && o>a) ||  (a<=m && o<=c && n<b && b<p && p<d) = [Rectangle (a,b) (c,d) , Rectangle (m,n) (a,p)]
--                      -- | b>=n, p>=d, m>a, o>c, m<c = [Rectangle (m,n) (o,p) , Rectangle (a,b) (m,d)]
--                      -- | a==m, n==d, o<c, d<p = [Rectangle (a,b) (o,p), Rectangle (o,b) (c,d)]
--                      -- | m==a, b==p, c<o, p<d =[Rectangle (m,n) (c,d), Rectangle (c,n) (o,p)]
--                      -- | m==a, b == n, c<o, p<d= [Rectangle (m,n) (o,p), Rectangle (a,p) (c,d)]
--                      -- | a==m, b==n, o<c, d<p = [Rectangle (a,b) (c,d), Rectangle (a,d) (o,p)]
--                      | otherwise = [Rectangle (a,b) (c,d) , Rectangle (a1,b1) (c1,d1)]




-- callIntersect :: [[Rectangle]]->[[Rectangle]]
-- callIntersect [] = []
-- callIntersect (x:xs) = (intersectRec (head x) (last x)) : callIntersect xs
--  --schimba cazurile de interesctare

-- verifyRect :: [Rectangle] -> [Rectangle]
-- verifyRect rs 
--             | rs == bn = rs
--             | otherwise = verifyRect bn
--             where
--                 ls = combineRec rs
--                 sn = callIntersect ls
--                 tn = concating sn
--                 bn = noDuplicateElem tn