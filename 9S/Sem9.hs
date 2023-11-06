module Sem9 (stackMachine) where

import Utils

-- Task 1
newtype Stack = Stack [Int]

emptyStack :: Stack
emptyStack = Stack []

push :: Stack -> Int -> Stack
push (Stack stack) x = Stack (x : stack)

pop :: Stack -> (Int, Stack)
pop (Stack []) = error "empty"
pop (Stack (x:xs)) = (x, Stack xs)

-- Task 2
data Instruction =
    Push Int
  | Add
  | Sub
  | Div
  | Mul
  | Pow
  deriving Show
  
computeInstr :: [Instruction] -> Stack -> Int
computeInstr [] (Stack [res]) = res
computeInstr (Push x : rest) stack = computeInstr rest (push stack x)
computeInstr (Add : rest) stack = computeInstr rest (add' stack)
computeInstr (Sub : rest) stack = computeInstr rest (sub' stack)
computeInstr (Div : rest) stack = computeInstr rest (div' stack)
computeInstr (Mul : rest) stack = computeInstr rest (mul' stack)
computeInstr (Pow : rest) stack = computeInstr rest (pow' stack)

computeInstructions :: [Instruction] -> Int
computeInstructions instructions = computeInstr instructions (Stack [])
    
add' :: Stack -> Stack
add' (Stack (x:y:rest)) = Stack (x + y : rest)
add' _ = error "err"

sub' :: Stack -> Stack
sub' (Stack (x:y:rest)) = Stack (y - x : rest)
sub' _ = error "err"

div' :: Stack -> Stack
div' (Stack (x:y:rest)) = Stack (y `div` x : rest)
div' _ = error "err"

mul' :: Stack -> Stack
mul' (Stack (x:y:rest)) = Stack (x * y : rest)
mul' _ = error "err"

pow' :: Stack -> Stack
pow' (Stack (x:y:rest)) = Stack (y ^ x : rest)
pow' _ = error "err"

-- вопрос: почему ghci агрится на патерн матчинг перед основной фукнцией ? потому что я error юзаю ?
-- например вот так : 
-- pow' :: Stack -> Stack
-- pow' _ = error "err"
-- pow' (Stack (x:y:rest)) = Stack (y ^ x : rest)

-- Task 3
parseInstr :: String -> Instruction
parseInstr s
  | strIsNumber s = Push (strToInt s)
  | s == "+" = Add
  | s == "-" = Sub
  | s == "*" = Mul
  | s == "/" = Div
  | s == "^" = Pow
  | otherwise = error "err"
  
parseString :: String -> [Instruction]
parseString input = map parseInstr (split input ' ')

-- Task 4
class Parsable a where
  parse :: a -> [Instruction]

instance Parsable String where
  parse input = parseString input

instance Parsable [String] where
  parse input = concatMap parseString input

instance Parsable [Instruction] where
  parse input = input

stackMachine :: (Parsable a) => a -> Int
stackMachine input = computeInstructions (parse input)
