import Data.Char

--q1
paraMaiuscula :: String -> String
paraMaiuscula x = [toUpper c | c <- x]

--q2
divisores :: Int -> [Int]
divisores 0 = []
divisores n = [length [x | x <- [1 .. n], mod n x == 0 ]]


isPrime :: Int -> Bool
isPrime n = ((divisores n)!!0 == 2)


--q3
menorLista :: [Int] -> Int
menorLista [x] = x
menorLista (x:y:xs)
    | x > y = menorLista (y:xs)
    | otherwise = menorLista (x:xs)

--q4
fib :: Integer -> Integer
fib 0  = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibTable :: Integer -> String
fibTable 0 = "n fib n\n0 0"
fibTable n = fibTable (n - 1)  ++ "\n" ++ show n ++ " " ++ show (fib n)


--q5
measure :: [a] -> Int
measure [] = -1
measure [_] = 1
measure (x:xs) = 1 + measure xs


--q6
-- [1, 3, 4, 5, 5]
takeFinal :: [a] -> Int -> [a]
takeFinal [] _ = []
takeFinal (x:xs) n
    | n == length (x:xs) = x:xs
    |otherwise = takeFinal xs n


--q7

-- essa solução não resolve o problema
-- checkIndex :: Eq a => a -> Int -> [a] -> Int
-- checkIndex _  _ [] = -1
-- checkIndex n i (x:xs)
--     | n == x = i
--     |otherwise = checkIndex n (i + 1) xs

-- remove :: Int -> [a] -> [a]
-- remove _ [] = []
-- remove index xs = [k | k <- xs, (checkIndex k 0 xs) /= index]

--- lado esquerdo [elem] lado direito
--- takeFirsts                  takeFinal xs ((length xs) - n - 1)

takeFirsts :: [a] -> Int -> [a]
takeFirsts [] _ = []
takeFirsts (x:xs) n
    | n /= 0 = x: takeFirsts xs (n - 1)
    | otherwise = []


remove :: Int -> [a] -> [a]
remove _ [] = []
remove index xs = takeFirsts xs index ++ takeFinal xs ((length xs) - index - 1)



--q8
firstInt :: [Int] -> Int
firstInt [] = 0
firstInt xs = head xs + 1

firstInt2 :: [Int] -> Int
firstInt2 [] = 0
firstInt2 (x:xs) = x + 1


--q9
add2 :: [Int] -> Int
add2 [] = 0
add2 [x] = x
add2 (x:y:ys) = x + y

add22 :: [Int] -> Int
add22 [] = 0
add22 (x:xs) = x + head xs


--q10
produto :: [Int] -> Int
produto [] = 1
produto (x:xs) = x * (produto xs)

--q11
unique :: [Int] -> [Int]
unique [] = []
unique x:xs 