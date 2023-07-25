-- data Maybe a = Nothing | Just a
--     deriving ( Eq , Ord , Read , Show)

--q1
divSegura :: Int -> Int -> Maybe Int
divSegura x y
    | y == 0 = Nothing
    |otherwise = Just (x `div` y)

--data Either a b = Left a | Right b
--deriving ( Eq , Ord , Read , Show)


--q2
divSegura2 :: Int -> Int -> Either String Int
divSegura2 x y
    | (y == 0) = Left (show(x) ++ "/" ++ show(0))
    |otherwise = Right (x `div` y)


--q3
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of
    Just b -> b : mapMaybe f xs
    Nothing -> mapMaybe f xs

