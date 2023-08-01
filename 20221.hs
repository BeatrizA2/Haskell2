--q7
divide :: (Int -> Bool) -> [Int] -> ([Int], [Int])
divide f [] = ([], [])
divide f xs = (list1, list2)
  where
    list1 = filter f xs
    list2 = filter (not . f) xs

--D
somaSqrt :: [Double] -> Double
somaSqrt xs = foldr (+) 0 (map (sqrt) (filter (> 0) xs))


--E
mdc :: Int -> Int ->Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)


--A
-- data Lampada = Compacta String Int | Incandescente String Int
--   deriving (Read)

-- instance Show Lampada where
--   show (Compacta name pot) = "Compacta " ++ name ++ " " ++ show (pot)
--   show (Incandescente name pot) = "Incandescente " ++ name ++ " " ++ show (pot)



-- --B
-- data Lustre = Pendente Lampada | Barra Lustre Lustre
--   deriving (Read)

-- potencia :: Lustre -> Int
-- potencia (Pendente l) = potLamp l
-- potencia (Barra l1 l2) = (potencia l1) + (potencia l2)

-- --C
-- balanceado :: Lustre -> Bool
-- balanceado (Pendente l) = True
-- balanceado (Barra l1 l2) = (balanceado l1) && (balanceado l2) && (potencia l1 == potencia l2)

-- ---
-- unzip' :: [(Int, Int)] -> ([Int], [Int])
-- unzip' xs = foldr (\(x, y) (a, b) -> (x: a, y:b)) ([], []) xs


primos :: Int -> [Int]
primos n = prime [2..n]
  where
    prime [] = []
    prime (x:xs) = x : prime [k | k <- xs, k `mod` x /= 0]


fatores :: Int -> [Int]
fatores n = [k | k <- prime [2..n], n `mod` k == 0]
  where
    prime [] = []
    prime (x:xs) = x : prime [k | k <- xs, k `mod` x /= 0]

perfeitos :: Int -> [Int]
perfeitos n = 1: [k| k <- [2..n], (foldr (+) 0 (map (^2) (fatores k))) == k]