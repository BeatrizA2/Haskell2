--lista de exercícios
--1
import Data.List

-- lista de classes

--q1
findDifference :: Eq a =>  [a] -> [a] -> Maybe String
findDifference xs ys
    | (lenght xs) /= (lenght ys) = Just (show lenght xs ++ "/=" ++ show lenght ys)


primos :: [Int] -> [Int]
primos [] = []
primos (x:xs) = x: primos [k| k <- xs, k `mod` x /= 0]

primosN :: Int -> [Int]
primosN 1 = [1]
primosN n = 1: primos [2..n]


sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = sublistas xs ++ map (x:) (sublistas xs)

filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir [[]] _ = ([[]], 0)
filtrarEInserir xs n = (list, num)
    where
        list = [k | k <- xs, (foldr (+) 0 (filter odd k)) > (foldr (+) 0 (filter even k))]
        num = (maximum (map (foldr (+) 0) list))*n


data Pilha a = Vazia | Topo a (Pilha a)
    deriving (Show)

push :: a -> Pilha a -> Pilha a
push n Vazia = Topo n Vazia
push n (Topo k p) = Topo n (Topo k p)

pop :: Pilha a -> Pilha a
pop Vazia = Vazia
pop (Topo n p) = p

top :: Pilha a -> Maybe a
top Vazia = Nothing
topo (Topo n p) = Just n



data CInt = Conjunto [Int] deriving (Show)

onlyOne :: [Int] -> [Int]
onlyOne [] = []
onlyOne (x:xs) = x: onlyOne (filter (/= x) xs)

makeSet :: [Int] -> CInt
makeSet [] = Conjunto []
makeSet xs = Conjunto [k| k <- (onlyOne (sort xs))]

uniont :: CInt -> CInt -> CInt
uniont (Conjunto xs1) (Conjunto xs2) = makeSet (xs1 ++ xs2)

mapSet :: (Int -> Int) -> CInt -> CInt
mapSet f (Conjunto xs) = Conjunto (map f xs)

