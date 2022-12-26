{-
ZADANIE 1
Każde słowo złożone z liter a i b przekształcamy idąc od lewej
według następujących reguł: ab → a, ba → b, bb → a, aa → aaa (jeśli słowo ma
nieparzystą długość, ostatnią literę przepisujemy). Napisać funkcję dlugosc,
zwracającą liczbę iteracji powyższych reguł, które prowadzą do słowa złożonego
z samych liter a lub słowa długości mniejszej niż 2. Przykładowo:
dlugosc "abaaa" = 1, bo |ab|aa|a → |a|aaa|a
dlugosc "abba" = 2, bo |ab|ba| → |a|b| oraz |ab| → a
dlugosc "babba" = 3, bo |ba|bb|a → |b|a|a, |ba|a → |b|a oraz |ba| → b
-}

zad1 :: String -> Integer
zad1 slowo
    | (dlugosc < 2) || (concat (replicate dlugosc "a") == slowo) = 0
    | otherwise = zad1 (przetworz slowo) + 1
    where dlugosc = length (slowo)
    
przetworz :: String -> String
przetworz slowo
    | (length slowo <=1) = slowo
    | poczatek == "ab" = "a" ++ (przetworz koniec)
    | poczatek == "ba" = "b" ++ (przetworz koniec)
    | poczatek == "bb" = "a" ++ (przetworz koniec)
    | poczatek == "aa" = "aaa" ++ (przetworz koniec)
    where poczatek = take 2 slowo
          koniec = drop 2 slowo

{-
ZADANIE 2
Dla dwóch liczb całkowitych a, b > 1 oznaczamy przez val(a, b)
największą potęgę liczby b, która dzieli a, np. val(56, 2) = 3, bo 23 dzieli 56, ale
2
4 nie dzieli 56; podobnie val(56, 3) = 0, bo 31 nie dzieli 56. Napisać funkcję:
val :: Integer -> Integer -> Integer
która dla podanych a i b oblicza val(a, b).
(b) Za pomocą powyższej funkcji val napisać funkcję:
g :: Integer -> Integer -> [Integer]
która dla podanych k > 1 oraz v ­ 0 zwraca listę (nieskończoną, w dowolnej
kolejności) liczb naturalnych n > 1, takich że val(n, k) = v. Przykładowo:
g 2 0 = [3,5,7,9,...]
g 3 1 = [3,6,12,15,21,...]
-}

val :: Integer -> Integer -> Integer
val a b | mod a b /= 0 = 0
        | otherwise = 1 + val (div a b) b

g :: Integer -> Integer -> [Integer]
g k v = [n | n <- [2,3..], val n k == v]

{-
ZADANIE 3
Kłosem nazywamy strukturę danych o funkcjonalności przypominającej listę, która umożliwia dokładanie elementów 
na początek i na koniec w czasie stałym, a ponadto odczytanie elementów po kolei. Stworzyć typ Klos a
przechowujący elementy typu a. Zdefiniować funkcje:
wnpk :: Klos a -> a -> Klos a
wnkk :: Klos a -> a -> Klos a
k2list :: Klos a -> [a]
Funkcje mają, odpowiednio, wstawiać element na początek i na koniec kłosa
oraz zamieniać kłos na listę. W ostatniej funkcji nie nakładamy ograniczenia na
złożoność
-}

data Klos a = Empty | Node a (Klos a) deriving (Show)

wnpk :: Klos a -> a -> Klos a
wnpk Empty a = Node a (Empty)
wnpk xs a = Node a (xs)

k2list :: Klos a -> [a]
k2list Empty = []
k2list (Node x xs) = x:(k2list xs)

list2k :: [a] -> Klos a
list2k [] = Empty
list2k [x] = Node x Empty
list2k (x:xs) = (Node x (list2k xs))

wnkk :: Klos a -> a -> Klos a
wnkk Empty a = Node a (Empty)
wnkk xs a = list2k ((k2list xs)++[a])


