import Data.List
--q1
--a
primos :: [Int] -> [Int]
primos [] = []
primos (x:xs) = x: primos [k | k <- xs, k `mod` x /= 0]

--b
primosN :: Int -> [Int]
primosN 0 = []
primosN 1 = [1]
primosN n = 1 : primos [2..n]

--q2
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ map (x:) (sublistas xs)


--q3

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir [] _ = ([], 0)
filtrarEInserir xs n = (list, num)
    where
        list = [k | k <- xs, (foldr (+) 0 (filter odd k)) > (foldr (+) 0 (filter even k)) ]
        num = (maximum (map sumList list))*n

--q4
--a) 
data Pilha a = Vazia | Topo a (Pilha a)
    deriving (Show)


--b)
top :: Pilha a -> Maybe a
top Vazia = Nothing
top (Topo n p) = Just n

pop :: Pilha a -> Pilha a
pop Vazia = Vazia
pop (Topo n p) = p

push :: a -> Pilha a -> Pilha a
push x Vazia = Topo x Vazia
push x (Topo n p) = Topo x (Topo n p)

--q5
--a
poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli a b c = (\x -> a*(x ^ 2) + b*x + c)

--b
listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli xs = [poli a b c | (a, b, c) <- xs]

--c
appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli funcs xs = [(funcs !! i) (xs !! i)| i <- [0 .. (length xs - 1)]]
---or appListaPoli funcs xs = zipWith $ funcs xs

--q6
data CInt = Conjunto [Int]
    deriving (Show)

removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates (x:xs) = x: removeDuplicates (filter (/= x) xs)

makeSet :: [Int] -> CInt
makeSet [] = Conjunto []
makeSet xs = Conjunto [k| k <- removeDuplicates (sort xs)]

unionC :: CInt -> CInt -> CInt
unionC (Conjunto xs) (Conjunto ys) = makeSet (xs ++ ys)


mapSet :: (Int -> Int) -> CInt -> CInt
mapSet f (Conjunto xs) = Conjunto (map f xs)