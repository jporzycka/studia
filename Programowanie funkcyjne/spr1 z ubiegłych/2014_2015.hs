{-
ZADANIE 1
Napisać bezpunktowo funkcję numocc, która zlicza wystąpienia wskazanego elementu w podanych listach, tzn. 
numocc x [`1, `2, ...`n] = [a1, a2, ..., an],
gdzie ai to liczba wystąpień x w liście `i. Na przyklad
numocc 1 [[1, 2], [2, 3, 2, 1, 1], [3]] = [1, 2, 0].
Podać najogólniejszą możliwą sygnaturę.
-}

numocc :: Int -> [[Int]] -> [Int]
--numocc = map . (sum .) . map . (fromEnum .) . (==)
-- numocc x list = map (\a -> sum(map (\b -> fromEnum (b == x)) a)) list
-- numocc x = map (\a -> sum(map (\b -> fromEnum ((==) x b) ) a))
--numocc x = map (\a -> sum((map (fromEnum . (==) x)) a))
--numocc x = map (sum.(map (fromEnum . (==) x)))
--numocc x = map ((sum.)(map ( ((.) fromEnum) ((==) x))))
--numocc x = (map.(sum.).map.((.) fromEnum).(==)) x
numocc = map.(sum.).map.((.) fromEnum).(==)

{-
ZADANIE 2
Obliczenia z użyciem operacji dodawania, mnożenia i negacji na elementach typu a (będącego instancją klasy Num) 
będziemy reprezentować przez drzewo obliczen typu CT´a, zdefiniowane jako
data CT a = Empty | Leaf a | Join (CT a) Op (CT a),
przy czym Empty to drzewo puste, Leaf — liść drzewa zawierającego wartość typu a,
zaś Join — drzewo reprezentujące wykonanie operacji Op na obliczeniach z lewego
i prawego poddrzewa. Typ Op jest określony jako
data Op = Add | Mul | Neg.

(a) Napisać funkcję wf :: CT a → Bool, która dla podanego drzewa sprawdza, czy
jest ono poprawne, w tym sensie, że nie jest puste i nie zawiera poddrzew postaci
(Join Empty Add r) lub (Join ` Add Empty) i analogicznie dla Mul, oraz poddrzew
postaci (Join ` Neg r), gdzie oba drzewa `, r są niepuste.

(b) Napisać funkcję eval :: Num a ⇒ CT a → a, która dla podanego drzewa wyliczy
wartość reprezentowanego w nim obliczenia. Na przykład dla
T = (Join (Join (Leaf 3) Add (Leaf 2)) Mul (Join (Leaf 2) Neg Empty))
wartość eval T = −10. Je´sli drzewo nie jest poprawne, funkcja powinna sygnalizować błąd
-}

data Op = Add | Mul | Neg
data CT a = Empty | Leaf a | Join (CT a) Op (CT a)

wf :: CT a → Bool
wf Empty = False
wf drzewo | 

{-
ZADANIE 3
Napisać bezpunktowo funkcję h, która zwraca elementy podanej listy
znajdujące się na pozycjach o numerach parzystych, w takiej kolejności, w jakiej
występują w oryginalnej liście. Na przykład
h [0, 1, 2, 3, 4] = [0, 2, 4],
h "AlaMaKota" = "Aaaoa"
-}
 