-- Task 1
reverse :: [a] -> [a]
reverse = foldl(\acc x -> x : acc) []

-- Task 2
-- without foldl
evenOnly' :: [a] -> [a]
evenOnly' [] = []            
evenOnly' [x] = [x]          
evenOnly' (x : y : xs) = y : evenOnly' xs

-- with foldl
-- нашел встроенную функцию zip пока решал 4 задачу, только я не понимаю почему zip находится после foldl ? 
-- если по сути я сначала сделал zip а потом внутри foldl начал работать с результатом zip'а
evenOnly :: [a] -> [a]
evenOnly list = foldl (\acc (i, x) -> if even i then acc ++ [x] else acc) [] (zip [1..] list)

-- Task 3
for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (counter, acc) inc condition body
    | condition counter = for (inc counter, body counter acc) inc condition body
    | otherwise = acc

-- алгоритмическая сложность у функции sum через for = O(n)
-- алгоритмическая сложность у функции sum через foldl = O(n)
-- ну что можно сказать, что foldl похож на for ? , они имеют O(n) -> в этой задаче оба проходятся по всему списку.

-- Task 4
decartMultiply :: [a] -> [b] -> [(a, b)]
decartMultiply list1 list2 = [(x, y) | x <- list1, y <- list2]

-- Task 5
type BinaryRelation a = Eq a => [(a, a)]

-- Рефлексивно ли бинарное отношение rel на множестве m
--               m          rel
refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl m rel = all (\x -> (x, x) `elem` rel) m

-- Симметрично ли бинарное отношение rel на множестве m
--              m          rel
sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim m rel = all (\(x, y) -> (y, x) `elem` rel) rel

-- Транзитивно ли бинарное отношение на множестве m
--                m           rel
trans :: Eq a => [a] -> BinaryRelation a -> Bool
trans m rel = all isTransitive [(x, y, z) | x <- m, y <- m, z <- m]
  where
    isTransitive (x, y, z) = not ((x, y) `elem` rel && (y, z) `elem` rel) || (x, z) `elem` rel

-- поч это не работает ((((((((((((
-- trans m rel = all (\(x, y) -> all (\(z, w) -> (x, y) `elem` rel && (y, z) `elem` rel && (x, z) `elem` rel) rel) rel
