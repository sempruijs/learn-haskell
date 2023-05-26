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
 
