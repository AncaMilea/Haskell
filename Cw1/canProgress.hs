-- You are asked to implement a function that determines whether or not a University of Southampton
-- undergraduate student has passed their second year and can progress to year 3 according to the standard
-- progression regulations given in section IV of the Calendar. Each module result is provided via a value of
-- type data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show, and the second year outcomes
-- are represented by a list of such values. For example canProgress [(ModuleResult 40.0 50), (ModuleResult 20.0
-- 50)] = True whereas canProgress [(ModuleResult 20.0 50), (ModuleResult 20.0 50), (ModuleResult 20.0 30)] = False as
-- there are too many credits of failed modules for compensation to apply. You may assume that each module
-- has a pass mark of 40, the qualifying mark in each case is 25, and none of the modules is core. At most 15
-- credits of failed modules at or above the qualifying mark may be compensated, in which case they are also
-- awarded credit. Note that modules may have credit weightings other than 7.5 ECTS, provided there are at
-- least 60 ECTS credits in total. If fewer credits than this have been taken, then the student has not yet passed
-- and may not progress. For simplicity, assume the function is given the set of marks after any referral or
-- repeat attempts replace earlier results, and with such marks already capped according to the regulations.
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