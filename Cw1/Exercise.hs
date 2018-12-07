-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
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
               

-- Exercise 2
-- longest common sub-list of a finite list of finite list
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
-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

getCredit :: ModuleResult->Float
getCredit (ModuleResult{credit=c, mark=m})= read(show c) ::Float

getMark :: ModuleResult->Int
getMark (ModuleResult{credit=c, mark=m})= read(show m) ::Int

canProgress :: [ModuleResult] -> Bool
canProgress ms 
               | (compC>0) && ((sumC+15)==totalC) = True
               | (compC==0) && (sumC==totalC) =True
               | otherwise = False
            where
                totalC= totalCreditsProgress ms
                sumC= sumCredits ms
                compC= compensateModule ms

totalCreditsProgress :: [ModuleResult]->Float
totalCreditsProgress [] =0
totalCreditsProgress (m:ms) = (getCredit m)+ (totalCreditsProgress ms)

compensateModule :: [ModuleResult] -> Int
compensateModule ms = length[x | x<-ms, (getMark x) >=25, (getMark x) <40]

sumCredits :: [ModuleResult] -> Float
sumCredits []=0
sumCredits (m:ms) 
                 | ((getMark m) >=40) = (getCredit m) + sumCredits ms
                 | otherwise= sumCredits ms 

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms 
           | (length ms)>=3 = classification ms 
           | otherwise = error "wrong input"

classification :: [[ModuleResult]] -> DegreeClass
classification ms 
           | (result >=40 && result<=49) = Third
           | (result >=50 && result<=59) = LowerSecond
           | (result >=60 && result<=69) = UpperSecond
           | (result >=70) = First
         where
            result= verifyUp ms

verifyUp :: [[ModuleResult]] ->Int
verifyUp ms
          | totalCredits>=68, totalCredits<70 = calcCreditUp (drop 1 ms) 70 totalCredits
          | totalCredits>=58, totalCredits<60 = calcCreditUp (drop 1 ms) 60 totalCredits
          | totalCredits>=48, totalCredits<50 = calcCreditUp (drop 1 ms) 50 totalCredits
          | otherwise = totalCredits
        where 
          totalCredits= degreeName (avgModuleYear ms)

calcCreditUp ::[[ModuleResult]]->Int->Int->Int
calcCreditUp ms least totalmarks
           | mark >= cr = least
           | otherwise = totalmarks
          where
            cr= round(cr1) `div` 2
            mark= searchUp ms least 1
            cr1= (totalCredits ms 1)


searchUp [] least part = 0
searchUp (m:ms) least part= (calcModuleUp m least)*part + searchUp ms least 2

calcModuleUp :: [ModuleResult] -> Int ->Int
calcModuleUp [] least=0;
calcModuleUp (m:ms) least
               | (getMark m)>=least = round(getCredit m) + calcModuleUp ms least
               | otherwise = calcModuleUp ms least

degreeBSc :: [[ModuleResult]] -> Int
degreeBSc ms = (((calcModule (ms !! 1))) + (calcModule (ms !! 2)*2)) `div` round(tot)
          where
            tot= totalCredits  (drop 1 ms) 1;

degreeMaster :: [[ModuleResult]] -> Int
degreeMaster ms = (((calcModule (ms !! 1) )) + (((calcModule (ms !! 2))*2)) + ((calcModule (ms !! 3))*2)) `div` round(tot)
          where
            tot= totalCredits  (drop 1 ms) 1;

totalCredits :: [[ModuleResult]]->Int->Float
totalCredits [] n = 0
totalCredits (m:ms) n = (calcCredit m)*fromIntegral(n) +totalCredits ms 2

calcModule :: [ModuleResult] -> Int
calcModule (m:ms) = (getMark  m)*round(getCredit m)

degreeName :: [[ModuleResult]] -> Int
degreeName ms 
             | ((length ms) == 3) = degreeBSc ms
             | ((length ms) == 4) = degreeMaster ms

calcCredit :: [ModuleResult] -> Float
calcCredit [] = 0
calcCredit (c:cs) = (getCredit  c) + calcCredit cs

calcModuleY :: [ModuleResult] -> Int
calcModuleY [] = 0
calcModuleY (m:ms) = ((getMark m) * round(getCredit m)) +calcModuleY ms

avgModuleYear :: [[ModuleResult]] ->[[ModuleResult]]
avgModuleYear [] = []
avgModuleYear (x:xs) 
              | ((length x)==1) = x : avgModuleYear xs
              | otherwise = [ModuleResult c m ] : avgModuleYear xs
             where
                c= calcCredit x
                m= (calcModuleY x) `div` round(c)





-- Exercise 5
-- search for the local maximum of f between x and x' using an 
-- approximation margin eps 
rad1 = ((sqrt(5)-1)/2) 
rad2 = ((3-sqrt(5))/2)

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps = gssrec d x x' eps (x' - x)

gssrec :: (Float->Float) ->Float -> Float -> Float -> Float -> Float 
gssrec f a b eps h  
                 | h1 <= (sqrt eps) = (new_a+new_b) / 2
                 | fc1 >fd1 = gssrec f new_a d1 eps (h1*rad1)
                 | otherwise = gssrec f c1 new_b eps (h1*rad1)
              where
                  new_a = min a b
                  new_b = max a b 
                  h1 = abs(new_b-new_a)
                  c1 = (new_a+(rad2*h1))
                  d1 = (new_a+(rad1*h1))
                  fc1 = f c1
                  fd1 = f d1 


-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillClimb equat x x' eps
                 where
                    equat= poly xs

powersX :: Num a => a -> [a]
powersX x = map (x^) [0..]

poly :: Num a => [a] -> a -> a
poly p = \x -> -(sum (getTogether (*) p (powersX x)))^2

getTogether :: (a->b->c)->[a]->[b]->[c]
getTogether f [] _ = []
getTogether f _ [] = []
getTogether f (x:xs) (y:ys) = (f x) y : getTogether f xs ys

-- Exercise 7

data Instruction = Add | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns ins
                                | (length ns) <1 = []
                                | (length ins) <1 = ns
                                | otherwise = instr ns ins

instr :: [Int] -> [Instruction] ->[Int]
instr xs [] = xs
instr xs (i:ins)
            | i == Add = instr ((a+b):new_xs) ins
            | i == Multiply = instr (c:new_xs) ins
            | i == Duplicate = instr (x':xs) ins
            | i == Pop = instr (drop 1 xs) ins
        where
            x'= head xs
            new_xs = drop 2 xs
            a = head xs
            b= xs !! 1
            c= a*b

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence 2 = [Duplicate,Multiply]
optimalSequence n
                | odd n = Duplicate:(optimalSequence (n-1))++[Multiply]
                | even n = Duplicate:Multiply:(optimalSequence (n `div`2))

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = busyBeaver ls ns [last ns] []
             where
                ls= allOp ((length ns)-1) [] []

allOp:: (Eq a, Num a) =>a -> [Instruction] -> [[Instruction]] -> [[Instruction]]
allOp 0 xs xss= xs : xss
allOp n xs xss = allOp (n-1) (Pop : xs) (allOp (n-1) (Multiply: xs)  (allOp (n-1) (Add : xs) xss))

busyBeaver [] _ _ solss = solss
busyBeaver (xs:xss) ns maxi solss | executeInstructionSequence ns xs > maxi = busyBeaver xss ns (executeInstructionSequence ns xs) (xs:[])
                                  | executeInstructionSequence ns xs == maxi = busyBeaver xss ns maxi (xs:solss)
                                  | otherwise = busyBeaver xss ns maxi solss

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)


noDuplicateElemR []= []
noDuplicateElemR (x:xs) = x : [ k  | k <- noDuplicateElemR(xs), k /=x ]


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
                bn = noDuplicateElemR tn

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
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


-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s 
              |length s >0 = messageCode str
              |otherwise= error "There is no string"
           where
              str= messageDecrypt s

messageDecrypt :: String -> String
messageDecrypt xs= [x|x<-xs, x=='0' || x=='1']

messageCode :: String -> String
messageCode [] = []
messageCode xs
             |(take 2 xs) == "00" = 'a': messageCode (drop 2 xs)
             |(take 2 xs) == "01" = 'b': messageCode (drop 2 xs)
             |(take 2 xs) == "10" = 'c': messageCode (drop 2 xs)
             |(take 2 xs) == "11" = 'd': messageCode (drop 2 xs)

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream [] = []
differentStream ls = diff ls 0

diff:: [[Int]]->Int->[Int]
diff [] pos = []
diff (l:ls) pos
           | nr>0 =0:diff ls (pos+1)
           | nr ==0 =1:diff ls (pos+1)
           where
           nr = ((l !! pos))

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
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

-- Exercise 15
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