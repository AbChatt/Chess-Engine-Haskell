module BFS where

-- Question 1

knightNext :: Int -> (Int, Int) -> [(Int, Int)]
knightNext n (x,y) = knightHelper n moves
    where
        moves = [(x + 2, y - 1), (x + 2, y + 1), (x - 2, y - 1), (x - 2, y + 1), (x - 1, y - 2), (x - 1, y + 2), (x + 1, y - 2), (x + 1, y + 2)]

knightHelper :: Int -> [(Int, Int)] -> [(Int, Int)]
knightHelper n [] = []
knightHelper n (x:xs)
    | isValidMove n x = [x] ++ knightHelper n xs
    | otherwise = knightHelper n xs

isValidMove :: Int -> (Int, Int) -> Bool
isValidMove n (x, y)
    | x >= 1 && x <= n && y >= 1 && y <= n = True
    | otherwise = False

-- The quad function as in the handout
quad :: Int -> [Int]
quad i = [4*i + 1, 4*i + 2, 4*i + 3, 4*i + 4]


-- Question 2

bfs :: (a -> [a]) -> a -> [a]
bfs next x0 = bfsHelper next x0 []

bfsHelper :: (a -> [a]) -> a -> [a] -> [a]
bfsHelper f init [] = [init] ++ bfsHelper f init (f init) 
bfsHelper f init (x:xs) = [x] ++ bfsHelper f x (xs ++ (f x))


-- Question 3

iterDeep :: (a -> [a]) -> a -> [a]
iterDeep next x0 = [x0] ++ iterDeepHelper next x0 1

iterDeepHelper :: (Num b, Eq b) => (a -> [a]) -> a -> b -> [a]
iterDeepHelper f root depth = depthLimitedDFS f [root] depth ++ iterDeepHelper f root (depth + 1)

depthLimitedDFS :: (Num b, Eq b) => (a -> [a]) -> [a] -> b -> [a]
depthLimitedDFS f nodes 0 = nodes
depthLimitedDFS f nodes depth = depthLimitedDFS f children (depth - 1) 
    where
        children = foldr (\x r -> f x ++ r) [] nodes