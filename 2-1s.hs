-- Task 1
dupRecursive :: [a] -> [a]
dupRecursive [] = []
dupRecursive (x:xs) = x : x : dupRecursive xs

dupFoldr :: [a] -> [a]
dupFoldr = foldr (\x acc -> x : x : acc) []

-- Task 2
data PeanoNatural =
    Zero | Succ PeanoNatural
    
natToInt :: PeanoNatural -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n
