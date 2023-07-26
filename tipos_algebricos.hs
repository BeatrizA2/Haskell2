-- data Maybe a = Nothing | Just a
--     deriving ( Eq , Ord , Read , Show)

--q1
divSegura :: Int -> Int -> Maybe Int
divSegura x y
    | y == 0 = Nothing
    |otherwise = Just (x `div` y)

--data Either a b = Left a | Right b
--deriving ( Eq , Ord , Read , Show)


--q2
divSegura2 :: Int -> Int -> Either String Int
divSegura2 x y
    | (y == 0) = Left (show(x) ++ "/" ++ show(0))
    |otherwise = Right (x `div` y)


--q3
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of
    Just b -> b : mapMaybe f xs
    Nothing -> mapMaybe f xs


--q4
-- foldr function value_for_[] list
classifica :: [Either a b] -> ([a], [b])
classifica xs = foldr (\eitherVal (left, right) -> 
                        case eitherVal of
                            Left a -> (a:left, right)
                            Right b -> (left, b:right)
                      ) ([], []) xs


--q5
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- a)
valAtRoot :: Tree a -> Maybe a
valAtRoot Leaf = Nothing
valAtRoot (Node value _ _) = Just value


--b)
countNodes :: Tree a -> Int
countNodes Leaf = 0
countNodes (Node _ t1 t2) = 1 + countNodes t1 + countNodes t2

--c
leftest ::Eq a => Tree a -> Maybe a
leftest Leaf = Nothing
leftest (Node n tl tr)
    |tl == Leaf = Just n
    |otherwise = leftest tl

--d
-- função transformativa -> por elemento
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node n tl tr) = Node (f n) (mapTree f tl) (mapTree f tr)


--e
insertL :: a -> Tree a -> Tree a
insertL n Leaf = Node n Leaf Leaf
insertL n (Node k tl tr) = Node k (insertL n tl) tr


--f
tamanho :: Tree a -> Int
tamanho Leaf = 0
tamanho (Node n tl tr) = 1 + tamanho tl + tamanho tr

medida :: Tree a -> Tree Int
medida Leaf = Leaf
medida (Node _ Leaf Leaf) = Node 1 Leaf Leaf
medida (Node _ tl tr) = Node (1 + tamanho tl + tamanho tr) (medida tl) (medida tr)

--g
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ x Leaf = x
foldTree func x (Node n tl tr) = func n (foldTree func x tl) (foldTree func x tr)


--h
sumt :: Int -> Int -> Int -> Int
sumt x y z = x + y + z

treeSum :: Tree Int -> Int
treeSum tree = foldTree sumt 0 tree