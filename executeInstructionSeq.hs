-- Exercise 7
-- A zero-address stack-based computer has instructions Add, Multiply, Duplicate, and Pop belonging to the data
-- type Instruction. Define a function executeInstructionSequence which, given a stack and a list of instructions,
-- emulates each instruction and returns the resulting stack. The stack is represented as a list of integers, so the
-- function takes a list of integers and a list of instructions and returns a list of integers. The Add, and Multiply
-- instructions implement the arithmetic operations with these names, taking the first two values off the stack,
-- performing the operation, and then pushing the result back on to the stack. For example,
-- executeInstructionSequence [4, 5] [Add] = [9]. The Duplicate instruction takes the value on top of the stack and
-- pushes another copy of this onto the stack. The Pop instruction simply removes the value on the top of the
-- stack. Hence executeInstructionSequence [4, 5, 6, 7] [Pop, Duplicate] = [5, 5, 6, 7].

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