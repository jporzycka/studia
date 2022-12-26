{-
ZADANIE 1
Napisać funkcję pownum :: Integer → [Integer], która dla podanego
n ­ 1 zwraca (być może nieskończoną) listę składającą się ze wszystkich liczb
takich, że suma n-tych potęg cyfr równa jest tej liczbie. Zatem np.
pownum 5 = [1, 4150, ...]
gdyż 45 + 15 + 55 + 05 = 4150.
-}

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

sumPowers :: Integer -> Integer
sumPowers n = sum [x^n | x <- digs n]

pownum :: Integer -> [Integer]
pownum n = [x | x <- [1,2..], sumPowers x == x]

{-
ZADANIE 2
Napisać funkcję ps, która dla podanej listy zwraca listę zawierającą
wszystkie jej prefiksy — w kolejności od najkrótszego (jednoelementowego) do
najdłuższego (cała lista) — a następnie kolejno coraz krótsze sufiksy tej listy.
Przykładowo:
ps "Test" = ["T", "Te", "Tes", "Test", "est", "st", "t"]
ps [3,5,2] = [[3], [3,5], [3,5,2], [5,2], [2]]
Dla pustej listy wynik może być dowolny. W rozwiązaniu należy w istotny sposób
użyć funkcji foldl lub foldr.
-}

dlugoscListy :: [a] -> Int
dlugoscListy [] = 0
dlugoscListy [x] = 1
dlugoscListy lista = 1 + dlugoscListy (tail lista)

prefix :: [a] -> Int -> [[a]]
prefix _ 0 = []
prefix xs n = [take n xs] ++ prefix xs (n-1)

ps :: [a] -> [[a]]
ps [] = []
ps lista = reverse (prefix lista ((dlugoscListy lista)-1)) ++ [lista] ++ (foldl(\acc x -> acc ++ [reverse x]) [] (prefix (reverse lista) ((dlugoscListy lista)-1)))



{-
ZADANIE 3
Rododendronem nazywamy drzewo, w którym każdy wierzchołek
może mieć dowolną liczbę potomków (być może zero). Rododendron nie może
być pusty. Stworzyć typ Rd a, przechowujący elementy typu a w rododendronie,
i zdefiniować funkcje:
el :: Eq a => Rd a -> a -> Bool
subst :: Eq a => a -> a -> Rd a -> Rd a
rd2list :: Rd a -> [a]
Funkcje mają, odpowiednio: sprawdzać czy podany element należy do rododendronu, zamieniać wszystkie wystąpienia pierwszego podanego elementu na drugi
oraz zamieniać rododendron na listę zgodnie z porządkiem preorder
-}

-- contains [] y = True
-- contains (x:xs) y = elem x y && contains xs y

-- equals x y = contains x y && contains y x

-- data Rd a = Leaf a | Node1 (Rd a) a | Node2 (Rd a) a [Rd a] deriving (Show)

-- valuesPreOrder :: Rd a -> [a]
-- valuesPreOrder (Leaf a) = [a]
-- valuesPreOrder (Node1 subTree a) = [a] ++ valuesPreOrder subTree
-- valuesPreOrder (Node2 leftSubTree a (Node2 leftSubTree a rightSubTree)) = [a] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree

-- rd2list :: Rd a -> [a]
-- rd2list (Node2 leftSubTree a (Node2 leftSubTree a rightSubTree)) = [a] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree

-- el :: Eq a => Sdb a -> a -> Bool
-- el tree a = elem a (valuesPreOrder tree)

-- eq :: Eq a => Sdb a -> Sdb a -> Bool
-- eq tree1 tree2 = equals (valuesPreOrder tree1) (valuesPreOrder tree2)

-- -- TU JAK ZROBIC PREORDER
   


data Rd a = Node a [Rd a]

el :: Eq a => Rd a -> a -> Bool
el (Node x children) y = x == y || any (el y) children

subst :: Eq a => a -> a -> Rd a -> Rd a
subst x y (Node z children) = Node (if x == z then y else z) (map (subst x y) children)

rd2list :: Rd a -> [a]
rd2list (Node x children) = x : (concatMap rd2list children)
