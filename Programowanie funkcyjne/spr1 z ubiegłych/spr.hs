{-
zad 2019
Palindrom nazywamy zbalansowanym, je˙zeli składa si˛e on wył ˛acznie z liter
a i b, i liczba liter a jest równa liczbie liter b. Napisa´c funkcj˛e bp :: Int → [String], która
dla danego n zwróci list˛e zbalansowanych palindromów o długo´sci n.
-}

podlisty :: [a] -> [[a]]
podlisty [] = [[]]
podlisty (x:xs) = [x:podlista | podlista <- podlisty xs] ++ podlisty xs

dlugoscListy :: [a] -> Int
dlugoscListy [] = 0
dlugoscListy [x] = 1
dlugoscListy lista = 1 + dlugoscListy (tail lista)

podlistyDlugosci :: Int -> [a] -> [[a]]
podlistyDlugosci 0 lista = [[]]
podlistyDlugosci k lista = [podlista | podlista <- podlisty lista, dlugoscListy podlista == k]




bp :: Int -> [String]
bp n | n==0 = []
     | mod n 2 == 1 = []
     | otherwise = foldl (\lista podlista -> (sklej podlista) : lista) [] (podlistyDlugosci (div n 2) (poczList (div n 2)))


poczList n = ['a' | r <- [1..n]] ++ ['b' | r <- [1..n]] 
sklej x = x ++ reverse x



