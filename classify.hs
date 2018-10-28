-- Exercise 4
-- You are asked to implement a function that determines the degree outcome for a University of Southampton
-- undergraduate student who has successfully completed their studies. This could be First, UpperSecond,
-- LowerSecond, or Third, depending on their average, based on the regulations in section IV of the Calendar.
-- You can assume there have been either 3 or 4 years of study, and that each year’s marks are supplied by a list
-- of module results as in the previous exercise. This list will have one list of module results per year, so there
-- will be 3 entries for a three year degree, and 4 entries for a four year degree. For example classify
-- [[ModuleResult 60.0 45], [ModuleResult 60.0 45], [ModuleResult 20.0 45, ModuleResult 40.0 45]] = Third whereas
-- classify [[ModuleResult 60.0 45], [ModuleResult 20.0 55, ModuleResult 20.0 55, ModuleResult 20.0 55],
-- [ModuleResult 60.0 65], [ModuleResult 60.0 65]] = UpperSecond due to the different weightings of each year.
-- You may assume that each year has been successfully passed. Note that modules may have weights other
-- than 7.5 ECTS credit weightings and different years could, in principle, have different credit totals, each of
-- which is 60 ECTS credits or more and finally that the degree classification may depend on more than the
-- final average in certain situations. For simplicity, you are not expected to include ordinary degrees in your
-- solution.
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

getCredit :: ModuleResult->Float
getCredit (ModuleResult{credit=c, mark=m})= read(show c) ::Float

getMark :: ModuleResult->Int
getMark (ModuleResult{credit=c, mark=m})= read(show m) ::Int

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
            result= degreeName (avgModuleYear ms)

degreeBSc :: [[ModuleResult]] -> Int
degreeBSc ms = ((calcModule (ms !! 1))`div` 3) + (calcModule (ms !! 2)*2)`div`3

degreeMaster :: [[ModuleResult]] -> Int
degreeMaster ms = ((calcModule (ms !! 1) )`div` 5) + (((calcModule (ms !! 2))*2)`div`5) + ((calcModule (ms !! 3))*2)`div`5

calcModule :: [ModuleResult] -> Int
calcModule [] = 0
calcModule (m:ms) = (getMark  m) + calcModule ms

calcCredit :: [ModuleResult] -> Float
calcCredit [] = 0
calcCredit (m:ms) = (getCredit  m) + calcCredit ms

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

degreeName :: [[ModuleResult]] -> Int
degreeName ms 
             | ((length ms) == 3) = degreeBSc ms
             | ((length ms) == 4) = degreeMaster ms

