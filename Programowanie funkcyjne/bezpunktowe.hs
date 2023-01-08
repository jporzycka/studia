greaterThan :: (Ord a) => [a] -> a -> [a]
greaterThan = (flip (.) (flip (>))) . (flip filter)

sumSquares :: (Num a) => [a] -> a
sumSquares = sum . map (^2)