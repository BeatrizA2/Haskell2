
-- 22 de junho
--questão 1
dobro :: Int -> Int
dobro n = 2*n

--questão 2
quadruplo :: Int -> Int
quadruplo n = 2*(dobro n)

--questão 3
poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a*(x^2) + b*x + c

--questao 4
ePar :: Int -> Bool
ePar n = ((n `mod` 2) == 0)

parImpar :: Int -> String
parImpar n
    | (ePar n == True) = "par"
    |otherwise = "impar"

--questao 5 -> três definições
maxThree :: Int -> Int -> Int -> Int
maxThree x y z
    | x >= y && x >= z = x
    | y >= z = y
    |otherwise = z

maxFour :: Int -> Int -> Int -> Int -> Int
maxFour x y z w
    | (maxThree x y z) >= w = maxThree x y z
    | otherwise = w


maxFour2 :: Int -> Int -> Int -> Int -> Int
maxFour2 x y z w = max (max (max x y) z) w

maxFour3 :: Int -> Int -> Int -> Int -> Int
maxFour3 x y z w = max (maxThree x y z) w


--questão 6
quantosIguais :: Int -> Int -> Int -> Int
quantosIguais x y z
    | x == y && x == z = 3
    | x == y || y == z || x == z  = 2
    | otherwise = 0


--questão 7
ehZero :: Int -> Bool
ehZero 0 = True
enZero _ = False

--questão 8  -> recursão de cauda
aux :: Int -> Int -> Int
aux 0 n = n
aux n x = aux (n - 1) (n + x)

sumTo :: Int -> Int
sumTo n = aux n 0


--questão 9
auxPot :: Int -> Int -> Int -> Int
auxPot _ 0 parcial = parcial
auxPot n k parcial = auxPot n (k - 1) (parcial*n)

potencia :: Int -> Int -> Int
potencia n k = auxPot n k 1

--questão 10
bin :: Int -> Int -> Int
bin _ 0 = 1
bin 0 _ = 0
bin n k = (bin (n - 1) k)  + (bin (n - 1) (k - 1))

--questao 11
-- 9 4 3 2
auxTri :: Int -> Int -> Int -> Int -> Int -> Int
auxTri n1 n2 n3 index n
    |index == n = n1
    |otherwise = auxTri (n1 + n2 + n3) n1 n2 (index + 1) n

tri :: Int -> Int
tri 1 = 1
tri 2 = 1
tri 3 = 2
tri n = auxTri 2 1 1 3 n


--questão 12
aux :: Int -> String -> String
aux 0 x = x
aux n x = aux (n-1) (x ++ " ")

addEspacos :: Int -> String
addEspacos n = aux n " "

--questão 13

auxRight :: Int -> String -> String
auxRight 0 x = x
auxRight n x = auxRight (n-1) (" " ++ x) 

paraDireita :: Int -> String -> String
paraDireita n x = auxRight n x

--questão 14

printRow :: String -> String -> IO()
printRow x y = putStrLn (x ++ "           " ++ y)

iterateList :: [Int] -> Int -> Int -> IO()
iterateList list i l
    | i >= l = return () -- Base case: terminate the recursion when i >= l
    | otherwise = do
        let index = show i
        let v = list !! i
        let value = show v
        printRow index value
        iterateList list (i + 1) l


--Recebe uma tabela com as vendas de cada semana
salesTable :: [Int] -> IO()
salesTable list = do
    printRow "Semana" "Venda"
    let lenList = length list
    iterateList list 0 lenList
    let sumValues = sum list
    let avg = sumValues `div` lenList
    printRow "Total" (show(sumValues))
    printRow  "Média" (show(avg)) 




