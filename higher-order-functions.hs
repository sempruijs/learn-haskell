applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f a b = f b a

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\f x -> if x == y then True else f) False ys

maximum' :: (Ord a) => [a] -> a
maximum' ys = foldl1 (\acc x -> if x > acc then x else acc) ys

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

esrever :: [a] -> [a]
esrever [] = []
esrever xs = (last xs) : esrever (init xs)

sqrtSum :: Int
sqrtSum = length (takeWhile (<10000) sqrtSumList) + 1
  where 
    sqrtList = [sqrt x | x <- [1..]]
    sqrtSumList = scanl1 (+) sqrtList

mapOverFunctions = map ($ 4) $ map (*) [sqrt x | x <- [1..100]]

makeNegative :: (Num a) => [a] -> [a]
makeNegative = map (negate . abs)