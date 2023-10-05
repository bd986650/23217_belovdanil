-- Task 1 && Tail Recursion
fib' :: Int -> Int
fib' 1 = 1
fib' 2 = 1
fib' x = fib' (x - 2) + fib' (x - 1)

-- Tail Recursion
fibTailRecursion :: Int -> Int
fibTailRecursion n = fib'' n 0 1
  where
    fib'' 0 a _ = a
    fib'' n a b = fib'' (n - 1) b (a + b)

fibMod5 :: [Int]
fibMod5 = [fibTailRecursion(x) | x <- [1..], fibTailRecursion(x) `mod` 5 == 0]

-- Task 2
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

perimeter :: [(Double, Double)] -> Double
perimeter [p1, p2, p3] = distance p1 p2 + distance p2 p3 + distance p3 p1

-- Task 3
checkAllEq :: Eq a => [a] -> Bool
checkAllEq [] = True 
checkAllEq [_] = True
checkAllEq (x:y:xs) = x == y && checkAllEq (y:xs)

-- Task 4
minDistance :: [(Double, Double)] -> Double
minDistance points = minimum [distance p1 p2 | p1 <- points, p2 <- points, p1 /= p2]

