-- 1 Task
p1 :: (Double -> Double) -> (Double -> Double) -> Double -> Double
p1 f g p = max (f p) (g p)

-- 2 Task
p2 :: (Double -> Double) -> Double -> Double
p2 f p = p1 f exp p

-- 3 Task
p3 :: (Double -> Double) -> Double -> Double -> Double
p3 f p n | n < 0 = error "Error"
          | n == 0 = p
          | otherwise = p3 f (f p) (n - 1)
