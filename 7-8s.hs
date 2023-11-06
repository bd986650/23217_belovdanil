import Data.Array

newtype Matrix = Matrix (Array (Int, Int) Double)
   
example1 :: [((Int, Int), Double)]
example1 = [((0,0), 1), ((0,1), -2), ((0,2), 3),
            ((1,0), 4), ((1,1), 0), ((1,2), 6),
            ((2,0), -7), ((2,1), 8), ((2,2), 9)]

example2 :: [((Int, Int), Double)]
example2 = [((0,0), 13), ((0,1), 0),
            ((1,0), 0 ), ((1,1), 12)]

m1 = makeMatrix (3, 3) example1 -- m1 :: Matrix
m2 = makeMatrix (2, 2) example2 -- m2 :: Matrix

-- Task 1

------------
-- эту функцию взял с гпт, потому что мое решение не работало нормально(тогда и последущие таски бы слетели изза этой фукнции)
-- в гугле и на гитхабе 0 инфы полезной было про создание матриц на хаскеле (((
--            matrix size      ((i, j), element)      result
makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix (m, n) list =
  let bounds = ((0, 0), (m - 1, n - 1))
      default_value = 0.0
      list' = [((i, j), val) | ((i, j), val) <- list, i >= 0 && i < m, j >= 0 && j < n]
      arr = array bounds (list' ++ [((i, j), default_value) | i <- [0..m-1], j <- [0..n-1], not ((i, j) `elem` map fst list)])
  in Matrix arr
------------

(!!!) :: Matrix -> (Int, Int) -> Double
(Matrix arr) !!! index = arr ! index

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix arr) =
    let ((_, _), (i, j)) = bounds arr in (i, j)

matrixIndices :: Matrix -> [(Int, Int)]
matrixIndices (Matrix arr) = [(i, j) | i <- [0..a], j <- [0..b], arr ! (i, j) /= 0]
  where
    ((0, 0), (a, b)) = bounds arr
    
    
-- Task 2
--                i    j    element
type MtxElem = ((Int, Int), Double)

matrixFold :: (b -> MtxElem -> b) -> b -> Matrix -> b
matrixFold fun acc (Matrix arr) = foldl (\acc' i -> fun acc' (i, arr ! i)) acc (matrixIndices (Matrix arr))

matrixMap :: (MtxElem -> Double) -> Matrix -> Matrix
matrixMap fun (Matrix arr) = Matrix $ array (bounds arr) [(i, fun (i, arr ! i)) | i <- indices arr]


-- Task 3
instance Show Matrix where
  show (Matrix arr) = unlines [unwords [show (arr ! (i, j)) | j <- [0..a]] | i <- [0..b]]
    where
      ((0, 0), (a, b)) = bounds arr
      
-- Task 4
------------

-- Task 5
instance Eq Matrix where
  (Matrix arr1) == (Matrix arr2) = arr1 == arr2
  
-- Task 6 & 7
-------------
