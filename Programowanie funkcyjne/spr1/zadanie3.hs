{- 
Zadanie 3. Bluszcz skierowany to struktura danych, która pozwala na: doł ˛aczenie elementu (de), odczytanie elementu ostatnio doł ˛aczonego (oe), usuni˛ecie elementu ostatnio doł ˛aczonego (ue), podanie liczby elementów równych elementowi doł ˛aczonemu jako pierwszy (le)
oraz zamian˛e całej struktury na list˛e (bsk2l), przy czym dowolne dwa elementy doł ˛aczone
w nast˛epuj ˛acych po sobie operacjach de musz ˛a by´c s ˛asiadami na li´scie. Zamiana na list˛e powinna by´c wykonalna w czasie liniowym wzgl˛edem ł ˛acznej liczby elementów, natomiast pozostałe
operacje — w czasie stałym. Zdefiniowa´c typ Bsk a, słu˙z ˛acy do przechowywania elementów
typu a w bluszczu skierowanym, oraz nast˛epuj ˛ace funkcje, realizuj ˛ace opisane wy˙zej operacje
z odpowiedni ˛a zło˙zono´sci ˛a:
de :: Bsk a -> a -> Bsk a
oe :: Bsk a -> a
ue :: Bsk a -> Bsk a
le :: Eq a => Bsk a -> Integer
bsk2l :: Bsk a -> [a]
Funkcje oe, ue i le nie s ˛a zdefiniowane dla bluszczu pustego
-}

data Bsk a = Empty | Node a (Bsk a) deriving (Show, Eq)

de :: Bsk a -> a -> Bsk a
de Empty a = Node a (Empty)
de xs a    = Node a (xs)

oe :: Bsk a -> a
oe (Node x xs) = x

ue :: Bsk a -> Bsk a
ue (Node x xs) = xs 

bsk2l :: Bsk a -> [a]
bsk2l (Node x Empty) = [x]
bsk2l (Node x xs)    = x:(bsk2l xs)

equals :: Eq a => a -> [a] -> Integer
equals x0 [] = 0
equals x0 [x]    | x0 == x   = 1
                 | otherwise = 0
equals x0 (x:xs) | x0 == x   = 1 + (equals x0 xs) 
                 | otherwise = (equals x0 xs)

le :: Eq a => Bsk a -> Integer
le bsk = equals (head (bsk2l bsk)) (tail (bsk2l bsk))

