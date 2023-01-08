{-
OPIS PROJEKTU
Test Kuratowskiego jest metodą sprawdzania planarności grafów, 
która polega na szukaniu podgrafu, który jest grafem homeomorficznym 
do K5 (graf pełny o pięciu wierzchołkach) lub K3,3 (graf pełny 
dwudzielny o sześciu wierzchołkach, z których trzy są połączone 
z każdym z pozostałych trzech). Jeśli taki podgraf zostanie znaleziony,
to graf jest nieplanarny, w przeciwnym razie jest planarny.

Poniżej znajduje się implementacja testu Kuratowskiego.
-}

import System.IO

-- type Graph = [(Int, Int)] definiuje nowy typ danych, służący do 
-- przechowywania reprezentacji grafu.
type Graph = [(Int, Int)]


-- FUNKCJE POMOCNICZE

-- zwraca listę wierzchołków grafu, wyciągając pierwszy 
-- i drugi element każdej krotki w liście grafu i usuwając duplikaty.
vertices :: Graph -> [Int]
vertices graph = removeDuplicates (concatMap (\(x, y) -> [x, y]) graph)
  where
    removeDuplicates [] = []
    removeDuplicates (x:xs)
      | x `elem` xs = removeDuplicates xs
      | otherwise = x : removeDuplicates xs

-- sprawdza, czy graf zawiera krawędź łączącą wierzchołki x i y.
hasEdge :: Graph -> Int -> Int -> Bool
hasEdge graph x y = (x, y) `elem` graph || (y, x) `elem` graph

-- zwraca wszystkie podciągi podanej listy
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = let next = subsequences xs
                     in next ++ map (x:) next


-- CZĘŚĆ DLA GRAFU K5 (grafu pełnego o pięciu wierzchołkach)

-- zwraca listę wszystkich pięcioelementowych kombinacji wierzchołków bez powtórzeń:
fiveNodeCombinations :: [a] -> [[a]]
fiveNodeCombinations nodes = filter ((==5) . length) (subsequences nodes)

-- sprawdza, czy dla podanej kombinacji wierzchołków istnieją krawędzie łączące wszystkie 
-- jej wierzchołki
isFullGraph :: Graph -> [Int] -> Bool
isFullGraph graph nodes = all (\(x, y) -> hasEdge graph x y) [(nodes !! a, nodes !! b) | a <- [0..length nodes - 1], b <- [0..length nodes - 1], a /= b]

--  sprawdza, czy graf zawiera podgraf K5
hasK5 :: Graph -> Bool
hasK5 graph | length (vertices graph) < 5 = False
            | otherwise = any (isFullGraph graph) (fiveNodeCombinations (vertices graph))


-- CZĘŚĆ DLA GRAFU K3,3 (grafu pełnego dwudzielnego o sześciu wierzchołkach, 
-- z których trzy są połączone z każdym z pozostałych trzech)

-- zwraca listę wszystkich sześcioelementowych kombinacji wierzchołków bez powtórzeń:
sixNodeCombinations :: [a] -> [[a]]
sixNodeCombinations nodes = filter ((==6) . length) (subsequences nodes)

-- sprawdza, czy podana sześcioelementowa kombinacja wierzchołków tworzy
-- razem z istniejącymi krawędziami graf dwudzielny pełny
isBipartiteGraph :: Graph -> [Int] -> Bool
isBipartiteGraph graph nodes = all (\(x, y) -> hasEdge graph x y) [(nodes !! a, nodes !! b) | a <- [0..2], b <- [3..5]] &&
                               all (\(x, y) -> hasEdge graph x y) [(nodes !! a, nodes !! b) | a <- [3..5], b <- [0..2]] &&
                               all (\x -> not (hasEdge graph x x)) nodes &&
                               not (any (\(x, y) -> hasEdge graph x y) [(nodes !! a, nodes !! b) | a <- [0..2], b <- [0..2]]) &&
                               not (any (\(x, y) -> hasEdge graph x y) [(nodes !! a, nodes !! b) | a <- [3..5], b <- [3..5]])

-- PROBLEM - nie zgadza się typ, jak daję np [1,2,3,4,5,6] to działa a jak daję vertices graph (= [1,2,3,4,5,6]) to nie działa
-- sprawdza, czy graf zawiera podgraf K3,3
hasK3_3 :: Graph -> Bool
hasK3_3 graph | length (vertices graph) < 6 = False
              | otherwise = any (isBipartiteGraph graph) (sixNodeCombinations (vertices graph))


-- SPRAWDZANIE CZY GRAF JEST PLANARNY

hasKuratowskiSubgraph :: Graph -> Bool
hasKuratowskiSubgraph graph = hasK5 graph || hasK3_3 graph

isPlanar :: Graph -> Bool
isPlanar graph = not (hasKuratowskiSubgraph graph)


-- TESTY

-- main :: IO ()
-- main = do
--   let testGraph1 = [(1, 2), (2, 3), (3, 1)]
--       testGraph2 = [(1, 2), (2, 3), (3, 1), (1, 3)]
--       testGraph3 = [(1, 2), (2, 3), (3, 1), (1, 4), (4, 2)]
--       testGraph4 = [(1, 2), (2, 3), (3, 1), (1, 4), (4, 2), (2, 4)]
--       testGraph5 = [(1, 2), (2, 3), (3, 4), (4, 5), (5, 1), (1, 6)]
--       testGraph6 = [(1, 2), (2, 3), (3, 4), (4, 5), (5, 1), (1, 6), (6, 7), (7, 2)]
--       testGraph7 = [(1, 2), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5), (3, 4), (3, 5), (4, 5)] -- graf K5
--       testGraph8 = [(1, 4), (1, 5), (1, 6), (2, 4), (2, 5), (2, 6), (3, 4), (3, 5), (3, 6)] -- graf K3,3

--   print (isPlanar testGraph1)
--   print (isPlanar testGraph2)
--   print (isPlanar testGraph3)
--   print (isPlanar testGraph4)
--   print (isPlanar testGraph5)
--   print (isPlanar testGraph6)
--   print (isPlanar testGraph7)
--   print (isPlanar testGraph8)



main :: IO ()
main = do  
      let list = []
      handle <- openFile "graph.txt" ReadMode
      contents <- hGetContents handle
      let graph = map (\line -> let [a,b] = words line in (read a, read b)) (lines contents)
      print (isPlanar graph)
      hClose handle


