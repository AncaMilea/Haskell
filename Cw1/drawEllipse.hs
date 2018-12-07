-- Exercise 11
-- You are asked to generate a list of rectangles giving an image of an ellipse. The ellipse is defined by the
-- floating point input parameters xCentre yCentre a b and has a boundary logically consisting of those realvalued
-- points (x,y) satisfying the equation (x – xCentre)2/ a2 + (y – yCentre)2/ b2 = 1. In practice, not all such
-- points can be represented computationally. The image generated by these rectangles should contain the
-- (integer) coordinates of all points on or within the given ellipse. It is also expected that the list by the
-- drawEllipse function is a minimal one in the sense defined in the previous exercise.

data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = simplified
	        where
	        	simplified = simplifyRectangleList rs
	        	rs= calcTup x y a b 

calcTup :: Float -> Float -> Float -> Float -> [Rectangle]
calcTup x y a b= [Rectangle(x1,y1) (x2,y2) |x1<-interv1,y1<-interv2, x2<-interv1,y2<-interv2, ((((fromIntegral(x1) - x)^2)/(a^2)) + (((fromIntegral(y1) - y)^2)/(b^2)))<=1 && ((((fromIntegral(x2) - x)^2)/(a^2)) + (((fromIntegral(y2) - y)^2)/(b^2)))<=1 && ((((fromIntegral(x1) - x)^2)/(a^2)) + (((fromIntegral(y2) - y)^2)/(b^2)))<=1 && ((((fromIntegral(x2) - x)^2)/(a^2)) + (((fromIntegral(y1) - y)^2)/(b^2)))<=1]
                     where 
                        interv1 = [(ceiling(x-a)) .. (floor(x+a))]
                        interv2 = [(ceiling(y-b)) .. (floor(y+b))]




-- getRectanglefst :: Rectangle->(Int,Int)
-- getCredit (Rectangle (a,b) (c,d))= read(show (a,b)) ::(Int,Int)

-- simplifyRectangleList :: [Rectangle] -> [Rectangle]
-- simplifyRectangleList rs = []

-- makeAllPoints :: [Rectangle]->[(Int,Int)]
-- makeAllPoints [] = []
-- makeAllPoints (Rectangle (a,b) (c,d):xs) = [(x,y)| x<-[a..c], y<-[b..d]]++(makeAllPoints xs)


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
                 | length ps ==1 = ps
                 | otherwise  = tn
            where
            	ds= invalidRectangle rs
            	ps= checkPoint ds ds
                tn = checkFUllage ps ps
                

