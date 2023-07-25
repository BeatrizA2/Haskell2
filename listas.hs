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
takeFinal :: [a] -> Int