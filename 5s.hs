-- Task 1
-- With foldl
map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> acc ++ [f x]) []

-- With foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc ->  [f x] ++ acc) []

-- Task 2
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub'( filter (/= x) xs)

union' :: Eq a => [a] -> [a] -> [a]
union' arr1 arr2 = nub'(arr1 ++ arr2) 

intersection' :: Eq a => [a] -> [a] -> [a]
intersection' xs = filter (`elem` xs)

-- Task 3
compute :: [String] -> Double -> Double
compute [] p = p
compute (instr:r) p
    | instr == "inc" = compute r (p + 1.0)
    | instr == "dec" = compute r (p - 1.0)
    | instr == "double" = compute r (p * 2.0)
    | instr == "sqrt" = compute r (sqrt p)
    | instr == "halveIfPositive" = if p > 0 then compute r (p / 2.0) else compute r p
    | otherwise = compute r p

computeVector :: [String] -> [Double] -> [Double]
computeVector arr = foldl (\acc x -> acc ++ [compute arr x]) []

-- Task 4
-- Without filter
cleaner :: [String] -> [String]
cleaner [] = []
cleaner (instr:r)
    | instr == "inc" = instr : cleaner r
    | instr == "dec" = instr : cleaner r
    | instr == "double" = instr : cleaner r
    | instr == "sqrt" = instr : cleaner r
    | instr == "halveIfPositive" = instr : cleaner r
    | otherwise = cleaner r

-- With filter
cleaner' :: [String] -> [String]
cleaner' = filter (`elem` ["inc", "dec", "double", "sqrt", "halveIfPositive"])

-- Task 5
-- With foldr
optimizer' :: [String] -> [String]
optimizer' = foldr optimize []
  where
    optimize "inc" ("dec":xs) = xs
    optimize "dec" ("inc":xs) = xs
    optimize x xs = x : xs

-- Task 6
type Point = (Double, Double)

aboveGraph :: (Double -> Double) -> Point -> Bool
aboveGraph f (x, y) = y > f x

pointsAboveGraph :: (Double -> Double) -> [Point] -> [Point]
pointsAboveGraph f = filter (aboveGraph f)
