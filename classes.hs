import Data.Map (fromListWith, toList)

--q1
findDiffElem :: Eq a => [a] -> [a] -> Int -> Int
findDiffElem [] [] _ = -1
findDiffElem (x:xs) (y:ys) i
    | x /= y = i
    | otherwise = findDiffElem xs ys (i + 1)

findDifference :: (Eq a, Show a)  => [a] -> [a] -> Maybe String
findDifference xs ys
    | length xs /= length ys = Just $ show (length xs) ++ " /= " ++ show (length ys)
    | otherwise =
        case findDiffElem xs ys 0 of
            -1 -> Nothing
            index -> Just $ show (xs !! index) ++ " /= " ++ show (ys !! index)


--q2
data Vector = Vector Integer Integer Integer
    deriving Show

instance Eq Vector where
    (Vector x1 y1 z1) == (Vector x2 y2 z2) = (x1 == x2) && (y1 == y2) && (z1 == z2)


--q3
instance Num Vector where
    (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)
    (Vector x1 y1 z1) * (Vector x2 y2 z2) = Vector (x1*x2) (y1*y2) (z1*z2)
    abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
    signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
    negate (Vector x y z) = Vector (-x) (-y) (-z)


--q4
-- removeElem :: Eq a => a -> [a] -> [a]
-- removeElem _ [] = []
-- removeElem e (x:xs)
--     | (e == x) = removeElem e xs
--     | otherwise = x : removeElem e xs

-- countElem :: Eq a =>  a -> [a] -> Int
-- countElem _ [] = 0
-- countElem e (x:xs)
--     | e == x = 1 + countElem e xs
--     | otherwise = countElem e xs


-- freqs :: [a] -> [(Int, a)]
-- freqs [] = []
-- freqs (x:xs) = (num, x) : freqs (removeElem x (x:xs))
--     where
--         num = countElem x (x:xs)


freqs :: (Ord a) => [a] -> [(a, Int)]
freqs xs = toList $ fromListWith (+) [(x, 1) | x <- xs]


--q5
data ITree = ILeaf | INode Int ITree ITree
    deriving Show


instance Eq ITree where
    ILeaf == ILeaf = True
    (INode n1 tl1 tr1) == (INode n2 tl2 tr2) = (n1 == n2) && (tl1 == tl2) && (tr1 == tr2)


