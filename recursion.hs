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

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if x == y then True else elem' x ys



quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) 
  | x <= head xs = isSorted xs
  | otherwise = False

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort (a:b:xs) 
  | isSorted result = result
  | a > b = bubbleSort (b:bubbleSort (a:xs))
  | a <= b = bubbleSort (a:bubbleSort (b:xs))
  where result = a:b:xs
  
