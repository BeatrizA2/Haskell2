
rev :: [a] -> [a]
rev = foldr (\x xs -> xs ++ [x]) []
--rev = foldl(flip (:)) []

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : map (x :) (prefixes xs)

prefixes2 :: [a] -> [[a]]
prefixes2 = foldr (\x xs -> [x] : map (x :) xs) []

lagrange :: [(Float, Float)] -> Float -> Float

data Trie a = Leaf a | Node a [Trie a]
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
    where
        f' acc t = foldtrie f acc t


