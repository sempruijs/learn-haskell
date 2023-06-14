import Data.List

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub 

spell :: String -> String
spell word = intersperse '.' word