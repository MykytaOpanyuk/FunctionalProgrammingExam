import Data.Typeable

-- Exercise 1 --

genList :: Int -> [[Int]]
genList 1 = [[1], [2], [3]]
genList n = (map (1:) (genList (n-1))) ++ (map (2:) (genList (n-1))) ++ (map (3:) (genList (n-1)))


removeRedundantPair :: [[Int]] -> [[Int]]
removeRedundantPair [] = []
removeRedundantPair (x:xs) | (smallerThan4 x) = x : (removeRedundantPair xs)
                           | otherwise = removeRedundantPair xs

smallerThan4:: [Int] -> Bool
smallerThan4 [] = True
smallerThan4 (x:[]) = True
smallerThan4 (x:y:xs) | x+y > 4 = False
                      | otherwise = smallerThan4 xs

-- Exercise 2 --

-- If we use this function to check smth with list of Int
-- then function need receive Int type of input data
type Function = Int -> Bool

checkCond :: [Int] -> Function -> Bool
checkCond a b = True

main = do
    let res1 = removeRedundantPair $ genList 3
    putStrLn (show res1)
