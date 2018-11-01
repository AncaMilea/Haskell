-- Exercise 9
-- A busy beaver is an instruction sequence containing only Pop, Multiply and Add instructions that terminates
-- with the highest possible result. This result must be a single value contained in the returned stack, which
-- must have length 1. For example, the instruction sequence [Pop] gives the maximum result possible when
-- executed on the input [0,1], as the returned stack is [1]; the sequence [Add] also gives the same result. The
-- findBusyBeavers function returns the list of all busy beavers for the given input stack s. Hence a valid result
-- for this function would be findBusyBeavers [0,1] = [[Pop],[Add]], or indeed the same list in a different order.


-- getListofPossibleInstruction :: [Int] -> [[Instructio]]
  

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