
--[] ou x:xs são construtores


--generating a list
asc :: Int -> Int -> [Int]
asc n m
    | m < n = []
    | m == n = [m]
    | m > n = n : asc (n + 1) m


--fromEnum :: Char -> Int
-- toEnum :: Int -> Char
-- last and head - to get the last and the first element of the list
-- init - prints everything but the last one
-- tail - gets everything but the first one
-- length list
-- null list -> verifica se há elementos na lista
-- and e or -> lista de booleanos 

maxTres :: Int -> Int -> Int -> Int
maxTres x y z
    | x >= y && x >= z = x
    | y >= z = y
    |otherwise = z

--list comprehension
list = [2*x | x <- [1 ..5], x > 3]
list_tuples = [(x, y) | x <- [1, 2, 3], y <- ['a', 'c']]

--list patterns
sumK :: [Int] -> Int
sumK [] = 0
sumK (x:xs) = x + sumK xs

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    |mod x 2 == 0 = x : evens xs
    |otherwise = evens xs


--tuples
-- fst - pega o primeiro elemento da tupla
-- snd - pega o segundo elemento da tupla 

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x + y | (x , y) <- xs]

--verifica se há elementos iguais
elemt :: Eq a => a -> [a] -> Bool
elemt _ [] = False
elemt n (x:xs)
    | n == x    = True
    | otherwise = elemt n xs


nub :: (Eq a) => [a] -> [a]
nub  [] = []
nub (x:xs)
    | elemt x xs = nub xs
    |otherwise = x: nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [_] = True 
isAsc (x:xs)
    | x > head xs = False
    |otherwise = isAsc xs


hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = False
hasPath (x:xs) n1 n2 
    | (fst x == n1) && (snd x == n2) = True
    | (fst x == n1) = hasPath xs (snd x) n2
    |otherwise = hasPath xs n1 n2


--high order functions -> receives functions has parameters
func :: (a -> b) -> a -> b
func f x = f x 


add1 = (\x -> x + 1)


--map -> maps one map of type a to another list in type b

a = map (\x -> x + 1) [1, 2, 3, 4]

b = map (\(x, y) -> x+y) [(1, 2), (0, 9)]

--filter
-- filter :: (a -> Bool) -> [a] -> [a]

c = filter (\x -> x > 2) [1, 3, 5, 2]

d = filter (\(x, y) -> x /= y) [(1, 2), (2, 2), (3, 3)]

--currying
-- f :: a -> b -> c -> d  = f :: a -> (b -> (c -> d))

add2 = (\x y -> x + y)
add22 = (\x -> (\y -> x + y))

doubleList = map (\x -> 2*x)


--function composition -> usa o operador "."
-- f . g == (\x -> f (g x))
-- reverse . sort

-- operator $
-- f xs = map (\x -> x+1) (filter (\x -> x > 1) xs)
-- usando o dollar sign : f xs = map (\x -> x + 1) $ filter (\x -> x > 1) xs



-- folding (foldr, foldl)
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr func accumulator list
--- foldl func list accumulator -> starts with the leftmost element of the list

trying = foldr (+) 0 [1 .. 10]

count e = foldr (\x acc -> if e == x then acc + 1 else acc) 0


--Defining datatypes
data Calculation =
    Add Int Int | Sub Int Int | Mul Int Int | Div Int Int

data PeaNum =
    Succ PeaNum | Zero


calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y
calc (Mul x y) = x*y
calc (Div x y) = div x y

data Tree a = Leaf | Node (Tree a) a (Tree a)

incr :: PeaNum -> PeaNum
incr = Succ

decr :: PeaNum -> PeaNum
decr (Succ n) = n