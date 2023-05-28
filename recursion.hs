maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list pared"
maximum' [x] = x
maximum' (x:xs)
  | x >  maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "empty list pared"
minimum' [x] = x
minimum' (x:xs)
  | x < minTail = x
  | otherwise = minTail
  where minTail = minimum' xs
 
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) = x:take' (n-1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]
