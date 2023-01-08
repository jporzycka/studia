{-
ZADANIE 1
NapisaC funkcjE oddbins n generującą listę wszystkich ciągów binarnych o długości n, w których liczba jedynek jest nieparzysta. Ciągi reprezentujemy
w postaci list, zatem np.
oddbins 3 = [[1, 0, 0], [0, 1, 0], [0, 0, 1], [1, 1, 1]].
Kolejność ciągów w liście nie ma znaczenia. Zakładamy, że n >=­ 1.
-}

oddBins :: Integer -> [[Integer]]
oddBins n = [list | list <- binaries n, (sum list) `mod` 2 == 1]
binaries :: Integer -> [[Integer]]
binaries 1 = [[0] , [1]]
binaries n = [ new : old | new <- [0,1], old <- binaries (n-1)]

{-
ZADANIE 2
Napisać funkcję diffsums :: [[Int]] → [[Int]], która z wejściowej listy usuwa listy o powtarzającej się sumie. Na przykład
diffsums [[1, 2], [3, 4, 5], [3], [], [7, 5]] = [[1, 2], [3, 4, 5], []] lub [[7, 5], [3], []] itp.
Każda suma z wejściowej listy ma być reprezentowana w liście wynikowej przez
dokładnie jedną listę. W rozwiązaniu należy użyć funkcji foldl lub foldr. Kolejność
w liście wynikowej nie ma znaczenia, ale kolejność w blokach ma zostać zachowana.

diffsums :: [[Int]] -> [[Int]]
diffsums [podlista] = [podlista]
diffsums (x:xs) | czyTaSamaSuma x (head xs) = diffsums ([x]++(tail xs))
                | istnieje (czyTaSamaSuma x )
                | otherwise = [x] ++ diffsums xs

czyTaSamaSuma :: [Int] -> [Int] -> Bool
czyTaSamaSuma l1 l2 | sumaListy l1 == sumaListy l2 = True
                    | otherwise = False

foldl (\podlista1 podlista2 -> czyTaSamaSuma podlista1 podlista2) [] lista

if czyTaSamaSuma podlista1 podlista2
  then podlista1
  else 

istnieje :: (a -> Bool) -> [a] -> Bool
istnieje f xs = foldl(\x1 x2 -> x1 || f x2) False xs

-}

takesecond :: [([Int], Int)] -> [Int]
takesecond xs = map (\x -> snd x) xs 
 
deleteduplicates :: [([Int], Int)] -> [[Int]]
deleteduplicates [] = []
deleteduplicates (x:xs) 
    | not (elem (snd x) (takesecond xs)) = (fst x) : deleteduplicates xs
    | otherwise       = deleteduplicates xs
 
diffsums :: [[Int]] -> [[Int]]
diffsums xs = deleteduplicates (foldl (\acc x -> (x, sum x) : acc) [] xs)

{-
ZADANIE 3
Napisać bezpunktowo funkcję compref, podającą długość najdłuższego
zgodnego odcinka początkowego dwóch list, oraz podać najogólniejszą możliwą
sygnaturę. Na przykład
compref [1, 2, 3, 4, 5] [1, 2, 3, 0, 0] = 3
compref [1, 2, 3] [9, 9, 9, 9, 9, 9] = 0.
Wskazówka: Można korzystać ze standardowych funkcji listowych, w tym z funkcji
zip, która z dwóch list tworzy listę par, np. zip [1, 2][’a’, ’b’, ’c’] = [(1, ’a’),(2, ’b’)].
-}

compref :: (Eq a) => [a] -> [a] -> Int
--compref one two = length ((takeWhile (uncurry (==))) ((zip one) two))
--compref one two = (length.(takeWhile (uncurry (==))).(zip one)) two
--compref one = (((.) length).((.)(takeWhile (uncurry (==)))).zip) one
compref = ((.) length).((.)(takeWhile (uncurry (==)))).zip

