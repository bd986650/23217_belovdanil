-- 1 task
even' :: Int -> Bool
even' x = x `mod` 2 == 0

-- 2 task
odd' :: Int -> Bool
odd' x = x `mod` 2 /= 0

-- 3 task
fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x - 2)

-- 4 task
sumFib :: Int -> Int
sumFib 1 = 1
sumFib x | odd'(fib(x)) = sumFib(x-1) + fib(x) 
         | otherwise = sumFib(x-1)
    
-- 5 task
sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits 1 = 1
sumDigits x = (x `mod` 10) + sumDigits(x `div` 10)

magicNumber :: Int -> Int
magicNumber x = sumDigits(sumDigits(x))
