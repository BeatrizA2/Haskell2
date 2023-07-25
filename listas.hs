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