-- main = putStrLn "Hello, Bia!"

square :: Int -> Int
square x = x*x

welcome :: String
welcome = " teste"

printName name = putStrLn ("Olá " ++ name ++ ", seja bem vinde!" ++ welcome)

printNumber num = putStrLn(show num)

printUserInput = do
    putStrLn "Please enter your name: "
    name <- getLine
    putStrLn "Please enter your age: "
    age <- getLine
    let ageAsNumber = read age::Int 
    putStrLn ("Your name is " ++ name ++ " and you are " ++ age ++ " years old" ++ " and after 10 years you will be " ++ show (ageAsNumber + 10))


calculator = do
    putStrLn "Enter first number"
    firstNum <- getLine
    putStrLn "Enter second number"
    secNum <- getLine
    let firstNumber = read firstNum::Int
    let secNumber =  read secNum::Int
    let total = firstNumber + secNumber
    putStrLn("The total is: " ++ show total)


--lists
scores :: [Int]
scores = [79, 43, 21]

-- last and head - to get the last and the first element of the list
-- init - prints everything but the last one
-- tail - gets everything but the first one
printListComp = do
    print (scores !! 0)


--functions
travelToWork :: String -> IO ()
travelToWork weather = do
    if weather == "Sunny"
        then putStrLn "walking"
        else putStrLn "driving"


in_range :: Int -> Int -> Int -> Bool
in_range min max x = x >= min && x <= max

--save the result
in_range2 min max x = (lowerbound && upperbound)
    where
        lowerbound = (min <= x)
        upperbound = (max >= x)

--factorial function -> using guards -> has to have boolean expressions
fac :: Int -> Int
fac n
    | n <= 1 = 1
    | otherwise = n * fac (n - 1)


--patern matching
is_zero :: Int -> Bool
is_zero 0 = True
is_zero _ = False

--accumulators - tail recursive function
fac2 n = aux n 1
    where
        aux n acc
            | n <= 1 = acc
            | otherwise = aux (n - 1) (n*acc)


aux :: Int -> String -> String
aux 0 x = x
aux n x = aux (n-1) (x ++ " ")

auxRight :: Int -> String -> String
auxRight 0 x = x
auxRight n x = auxRight (n-1) (" " ++ x) 

addEspacos :: Int -> String
addEspacos n = aux n " "

paraDireita :: Int -> String -> String
paraDireita n x = auxRight n x

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




