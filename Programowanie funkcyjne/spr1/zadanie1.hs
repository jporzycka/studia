-- zad1

dlugoscListy :: [Int] -> Int
dlugoscListy []    = 0
dlugoscListy [x]   = 1
dlugoscListy lista = 1 + dlugoscListy (tail lista)

podlisty :: [Int] -> [[Int]]
podlisty []     = [[]]
podlisty (x:xs) = [x:podlista | podlista <- podlisty xs] ++ podlisty xs

czyPodciag :: [Int] -> [Int] -> Bool
czyPodciag [] _          = True
czyPodciag xs []         = False
czyPodciag [x] [y]       | x == y = True 
                         | otherwise = False
czyPodciag [x] (y:ys)    | x == y = True
                         | otherwise = False
czyPodciag (x:xs) (y:ys) | x == y = True && czyPodciag xs ys
                         | otherwise = False || czyPodciag (x:xs) ys

podciagi :: [Int] -> [[Int]]
podciagi lista = [podciag | podciag <- (podlisty lista), (czyPodciag podciag lista), podciag /= []]

przez4 :: [Int] -> Bool
przez4 lista = if mod (sum lista) 4 == 0 then True else False

najdl :: [[Int]] -> [Int]
najdl []  = []
najdl [x] = x
najdl (x:podciagi) | ((dlugoscListy x) > (dlugoscListy (head podciagi))) = najdl (x:(tail podciagi))
                   | otherwise = najdl podciagi

cztery :: [Int] -> Int
cztery lista = dlugoscListy (najdl [podciag |podciag <- (podciagi lista), przez4 podciag])

