-- Exercise 8
-- Assuming that the stack has the singleton initial value [x] then the instruction sequence [Duplicate, Multiply]
-- will leave the single value x2 on the stack. Likewise, [Duplicate, Duplicate, Duplicate, Multiply, Multiply, Multiply]
-- will produce x4. There is however a more efficient sequence for computing this result, namely [Duplicate,
-- Multiply, Duplicate, Multiply]. Write a function to give an optimal (ie shortest) instruction sequence for raising
-- the initial value x to the supplied parameter n, which you can assume is a positive integer. For example,
-- valid results for this function would include optimalSequence 1 = [], and optimalSequence 2 = [Duplicate,Multiply].
data Instruction = Add | Multiply | Duplicate | Pop deriving (Eq, Show)

optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence 2 = [Duplicate,Multiply]
optimalSequence n
                | odd n = Duplicate:(optimalSequence (n-1))++[Multiply]
                | even n = Duplicate:Multiply:(optimalSequence (n `div`2))
