{-
ZADANIE 1
Napisać funkcję rd :: Integer → [Integer], która dla podanego n ­>=1
zwraca (być może nieskończoną) listę składającą się ze wszystkich liczb k takich,
że suma dodatnich dzielników liczby k równa jest n + k. Zatem np. rd 1 zwraca
listę wszystkich liczb pierwszych, rd 2 daje listę pustą, a rd 3 to lista zawierająca
tylko liczbę 4.
W przypadku, gdy wynik jest listą skończoną, wskazane jest, by funkcja kończyła
działanie. Wskazówka: jeśli liczba k ma dzielnik d, to ma również dzielnik k/d.
Na tej podstawie można oszacować, że dla n > 1 w wyniku funkcji rd n nie
może być liczb większych niż n^2
-}

dzielniki :: Integer -> [Integer]
dzielniki 1 = []
dzielniki n = [d | d <- [1..(div n 2)], mod n d == 0]

rd :: Integer -> [Integer]
rd 1 = [k | k <- [1,2..], dzielniki k == []]
rd n = [k | k <- [1..(n^2+1)], sum (dzielniki k) == (fromIntegral n)]

{-
ZADANIE 2
Napisać funkcję repl :: Eq a ⇒ [a] → [(a, a)] → [a], która dla
danej listy L oraz listy par dokonuje zamian elementów w L w taki sposób, że
każde wystąpienie pierwszego elementu pewnej pary zostaje zamienione na drugi
element tej pary, np.
repl [1,2,3,1,2] [(2,4)] = [1,4,3,1,4]
repl "alamakota" [(’a’,’u’), (’o’,’e’)] = "ulumuketu"
W rozwiązaniu należy w istotny sposób użyć funkcji foldl lub foldr. Uwaga:
można założyć, że wszystkie elementy występujące w parach są różne, tzn. lista
k par (a1, b1), ...,(ak, bk) zawiera 2k różnych elementów a1, ..., ak, b1, ..., bk.
-}

repl :: Eq a => [a] -> [(a, a)] -> [a]
repl [] _ = []
repl lista [] = []
repl lista zmiany = (foldl(\acc x -> acc ++ [(czyZmieniac x zmiany)]) [] lista)


czyZmieniac :: Eq a => a -> [(a,a)] -> a
czyZmieniac x [] = x
czyZmieniac x zmiany = if x == x1 then x2
                       else czyZmieniac x (tail zmiany)
    where 
        x1 = (fst (head zmiany)) 
        x2 = (snd (head zmiany))

{-
ZADANIE 3
Słabym drzewem binarnym nazywamy strukturę, w której każdy
wierzchołek zawiera pewną wartość, a ponadto ma 0, 1 lub 2 wierzchołki potomne. W tym ostatnim przypadku kolejność potomków nie jest rozróżniana,
tzn. drzewa różniące się jedynie kolejnością potomków uznajemy za równe. Słabe drzewo binarne nie może być puste. Stworzyć typ Sdb a, przechowujący
elementy typu a w słabym drzewie binarnym, i zdefiniować funkcje:
el :: Eq a => Sdb a -> a -> Bool
eq :: Eq a => Sdb a -> Sdb a -> Bool
valuesPreOrder :: Sdb a -> [a]
Funkcje mają, odpowiednio: sprawdzać czy podany element należy do drzewa,
sprawdzać czy podane drzewa są równe oraz zamieniać drzewo na listę przeszukując je wszerz (kolejność przeglądania potomków może być dowolna)
-}

contains [] y = True
contains (x:xs) y = elem x y && contains xs y

equals x y = contains x y && contains y x

data Sdb a = 
    Node1 (Sdb a) a
    | Node2 (Sdb a) a (Sdb a)
    | Leaf a 
    deriving (Show)

-- instance Functor Sdb where
--     fmap = treeMap

-- treeMap :: (a -> b) -> Sdb a -> Sdb b
-- treeMap f (Leaf a) = Leaf (f a)
-- treeMap f (Node1 subTree a) = Node1 (treeMap f subTree) (f a)
-- treeMap f (Node2 leftSubTree a rightSubTree) = Node2 (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)

valuesPreOrder :: Sdb a -> [a]
valuesPreOrder (Leaf a) = [a]
valuesPreOrder (Node1 subTree a) = [a] ++ valuesPreOrder subTree
valuesPreOrder (Node2 leftSubTree a rightSubTree) = [a] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree

el :: Eq a => Sdb a -> a -> Bool
el tree a = elem a (valuesPreOrder tree)

eq :: Eq a => Sdb a -> Sdb a -> Bool
eq tree1 tree2 = equals (valuesPreOrder tree1) (valuesPreOrder tree2)
   

