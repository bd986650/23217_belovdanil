data BinTree a =
    Nil |
    Node {
        left :: BinTree a,
        right :: BinTree a,
        value :: a,
        count :: Int
    }

-- для более удобного вывода
instance Show a => Show (BinTree a) where
    show = show0 0 where
        show0 _   Nil = "Nil"
        show0 lvl Node{left=l, right=r, value=v, count=cnt} =
            "Node (v = " ++ show v ++ ")\n" ++
            replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
            replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"
            
testTree :: BinTree Int
testTree = Node {
    left = Node {
        left = Node {left  = Nil, right = Nil, value = 1, count = 1},
        right = Node {
            left = Node {
                left = Nil, right = Nil, value = 4, count = 1},
            right = Node {
                left = Nil, right = Nil, value = 7, count = 1},
            value = 6, count = 1
        },
        value = 3, count = 1
    },
    right = Node {
        left  = Nil,
        right = Node {
            left  = Node {
                left = Nil, right = Nil, value = 13, count = 1},
            right = Nil,
            value = 14, count = 1
        },
        value = 10, count = 1
    },
    value = 8, count = 1
}

-- Task 1
insert :: Ord a => BinTree a -> a -> BinTree a
insert Nil x = Node Nil Nil x 1
insert (Node left right value count) x
    | x > value = Node left (insert right x) value count
    | x < value  = Node (insert left x) right value count
    | otherwise  = Node left right value (count + 1)
    
fromList :: Ord a => [a] -> BinTree a
fromList = foldl insert Nil

-- Task 2
findMin :: Ord a => BinTree a -> Maybe a
findMin Nil = error "empty"
findMin (Node Nil _ value _) = Just value
findMin (Node left _ _ _) = findMin left

findMax :: Ord a => BinTree a -> Maybe a
findMax Nil = Nothing
findMax (Node _ Nil value _) = Just value
findMax (Node _ right _ _) = findMax right

-- Task 3
----------------

-- Task 4
findAny :: Ord a => (a -> Bool) -> BinTree a -> Maybe a
findAny _ Nil = error "empty"
findAny predicate (Node left right value _)
    | predicate value = Just value
    | not (predicate value) = findAny predicate left
    | otherwise = findAny predicate right

-- Task *
-- ну я понимаю как проверить является ли бинарное дерево деревом , но написать не знаю как , мб 0.5балла за это? (шутка офк)

-- Интересно кстати было бы узнать что лучше использовать Nothing или error "err"
-- я так понял nothing не завершит программу в отличии error. 
-- и вот вопрос что лучше использовать и практичней ?
-- (во второй задаче использовал то и то)
