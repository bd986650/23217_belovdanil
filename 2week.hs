-- 1 Task
f1 :: Double -> Double
f1 x = x

g1 :: Double -> Double
g1 x = x

p1 :: (Double -> Double) -> (Double -> Double) -> Double -> Double
p1 f1 g1 p | f1 p == g1 p = p
           | f1 p > g1 p = f1 p
           | otherwise = g1 p

-- 2 Task
exp' :: Double -> Double
exp' = exp

p2 :: (Double -> Double) -> Double -> Double
p2 exp' p | exp' p > p = exp' p
         | exp' p == p = p
         | otherwise = p


-- 3 Task
f3 :: Double -> Double
f3 x = x

p3 :: (Double -> Double) -> Double -> Double -> Double
p3 f3 p n = if n == 1 then f3 p else n * (f3 p)
