{-
ZADANIE 1
Rozważmy ciągi (an),(bn), które spełniają następującą zależność rekurencyjną
an = (n − 1)bn−1 − 3an−1
bn = 3bn−1 + (n − 1)2
an−1 − (n − 1)2
Dodatkowo wiemy, że a0 = b0 = 1. Napisać funkcję seqIndex m, która zwraca najmniejsze k takie, że 
a0 + a1 + . . . + ak ­ m. 
Na przykład seqIndex 100 >= 4, seqIndex 1000000
= 8.
-}

seqIndex :: Int -> Int
seqIndex m = head [check x m | x <- [1..]]

check :: Int -> Int -> Int
check n m = if sum [genA x | x <- [0..n]] >= m then n else check (n+1) m

genA :: Int -> Int
genA 0 = 1
genA n = (n-1)*(genB (n-1)) - (3*(genA (n-1)))

genB :: Int -> Int
genB 0 = 1
genB n = 3*(genB (n-1)) + ((n-1)^2)*(genA (n-1)) - (n-1)^2


{-
ZADANIE 2
Rozważmy typ danych
data Expr a = Value a
| Add (Expr a) (Expr a)
| Mul (Expr a) (Expr a)
| Sub (Expr a) (Expr a)
| P
przechowujący częściowe wyrażenia, tzn. wyrażenia, które zawierają operacje dodawania (Add), mnożenia (Mul) i odejmowania (Sub) 
oraz wartość P, która oznacza, iż konkretny argument nie jest jeszcze znany. Argumenty do operacji arytmetycznych przechowujemy 
za pomocą Value. Napisać funkcję eq :: (Eq a) → Expr a → Expr a → Bool,
która zwraca True, jeśli wyrażenia są takie same, i False w przeciwnym wypadku. Przyjmujemy, że dwa wyrażenia są takie same, 
jeśli jedno można otrzymać z drugiego przez zamiany P na dowolne inne wyrażenia. Na przykład
eq (Add (Value 1) (Value 2)) (Add (Value 1) (Value 3)) = False
eq (Add (Value 1) (Value 2)) P = True
-}

data Expr a = Value a | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a) | Sub (Expr a) (Expr a) | P


{-
ZADANIE 3
Napisać funkcję cykl, która dla podanej niepustej listy zwraca listę jej wszystkich przesunięć cyklicznych, w dowolnej kolejności. 
Podać najogólniejszą możliwą sygnaturę. Funkcja ma w nietrywialny sposób korzystać z foldl lub foldr, przy czym fold
musi stanowić najbardziej zewnętrzną część definicji, np. cykl ` = foldl ... Przykładowo, wywołanie cykl [1,2,3] 
powinno zwrócić [[1,2,3], [2,3,1], [3,1,2]] lub dowolną permutację takiej listy
-}

-- dlugoscListy :: [a] -> Int
-- dlugoscListy [] = 0
-- dlugoscListy [x] = 1
-- dlugoscListy lista = 1 + dlugoscListy (tail lista)

-- cykl :: [a] -> [[a]]
-- cykl [] = [[]]
-- cykl lista = foldl(\acc x ->  [(przesun (head acc))] ++ acc) [lista] [1..(dlugoscListy(lista)-1)]

-- przesun :: [a] -> [a]
-- przesun (x:xs) = xs ++ [x]

cykl :: [a] -> [[a]]
cykl xs = foldl (\acc x -> acc ++ [shift x acc]) [[]] xs
  where
    shift x acc = if null acc then [x] else x : (tail (head acc))

                  

