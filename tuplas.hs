---q1
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (min (min x y) z, max (max x y) z)

-- menorMaior2 :: Int -> Int -> Int -> (Int, Int)
-- menorMaior2 x y z = (menor, maior)
--     where
--         menor = if (x < y && x < z) then x else if (y < z) then y else z
--         maior = if (x >= y && x >= z) then x else if (y >= z) then y else z


--- q2
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (x, y, z) = (menor, meio, maior)
    where
        aux = menorMaior x y z
        menor = fst aux
        maior = snd aux
        meio = [k | k <- [x, y, z], k /= maior && k /= menor] !! 0


-- q3
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

fstCoord :: Ponto -> Float
fstCoord p = fst p

sndCoord :: Ponto -> Float
sndCoord p = snd p

isVertical :: Reta -> Bool
isVertical r = (fstCoord (fst r)) == (fstCoord (snd r))
 

 --q4
pontoY :: Float -> Reta -> Float
pontoY x r = (((y2 - y1) * (x - x1)) / (x2 - x1)) + y1
    where
        f = fst r
        s = snd r
        x1 = fstCoord f
        y1 = sndCoord f
        x2 = fstCoord s
        y2 = sndCoord s