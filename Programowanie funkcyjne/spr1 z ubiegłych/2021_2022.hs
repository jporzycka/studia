{-
ZADANIE 1

-}

{-
ZADANIE 2

-}

{-
ZADANIE 3

-}

{- 
ZADANIE 2 - 2021
-}

wIluListach :: Int -> [[Int]] -> [Int]
wIluListach 0 lista = [sumaListy (map (\podlista -> czyZawiera podlista 0) lista)]
wIluListach x lista = (sumaListy (map (\podlista -> czyZawiera podlista x) lista)):(wIluListach (x-1) lista)

sumaListy :: Num a => [a] -> a
sumaListy xs = foldl (+) 0 xs

czyZawiera :: [Int] -> Int -> Int
czyZawiera [] _ = 0
czyZawiera lista n | (head lista) == n = 1
                  | otherwise = czyZawiera (tail lista) n