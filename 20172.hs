--q3
aux :: (a -> b) -> (a -> b) -> [a] -> Int -> [b]
aux _ _ [] _ = []
aux f g (x:xs) counter
    | (counter `mod` 2 == 0) = (f x) : aux f g xs (counter + 1)
    | otherwise = (g x) : aux f g xs (counter + 1)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g xs = aux f g xs 0


--q5
data Mobile = Pendente Int | Barra Mobile Mobile

--a

peso :: Mobile -> Int
peso (Pendente p) = p
peso (Barra m1 m2) = (peso m1) + (peso m2)

--b
balanceado :: Mobile -> Bool
balanceado (Pendente _) = True
balanceado (Barra m1 m2) = (peso m1 == peso m2) && (balanceado m1) && (balanceado m2) 