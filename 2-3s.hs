import Control.Monad.State

-- 1
fact' :: State (Int, Int) Int
fact' = do
    (step, acc) <- get
    if step == 0
        then return acc
        else do
            put (step-1, acc*step)
            fact'

fact :: Int -> Int
fact n = evalState fact' (n, 1)

-- 2
fibb' :: State (Int, Int, Int) Int
fibb' = do
    (step, n1, n2) <- get
    if step == 0
        then return n1
        else do
            put (step-1, n1+n2, n1)
            fibb'

fibb :: Int -> Int
fibb n = evalState fibb' (n, 1, 0)

-- 3
data BinTree a =
    Nil |
    Node (BinTree a) a (BinTree a)
    deriving Show

numberTree' :: BinTree () -> State Integer (BinTree Integer)
numberTree' Nil = return Nil
numberTree' (Node left _ right) = do
    countLeft <- numberTree' left
    count <- get
    put (count + 1)
    countRight <- numberTree' right
    return (Node countLeft count countRight)

numberTree :: BinTree () -> BinTree Integer
numberTree tree = evalState (numberTree' tree) 0
