import Data.List
-- import System.IO


isEven x = x `mod` 2 == 0


deathTreat age  
  | age <= 40 = "chill"
  | age <= 70 = "the end is near"
  | age > 70 = "goodbye"


times4 x = x * 4


multi4 :: [Int] -> [Int]  
multi4 [] = []
multi4 (x:xs) = times4 x : multi4 xs


moep _ [] = []
moep f (x:xs) = f x : moep f xs

stringAreEq :: [Char] -> [Char] -> Bool
stringAreEq [] [] = True
stringAreEq  (x:xs) (y:ys) = x == y && stringAreEq xs ys
stringAreEq  _ _ = False

ferbie 0 = 1
ferbie 1 = 1
ferbie x = ferbie (x - 1) + ferbie (x  - 2)

data Customer = Customer String String Double
  deriving Show

tomSmith = Customer "Tom Smith" "123 main" 20.5

getBalans (Customer _ _ b) = b

biSection f r ipt min max
  | f ipt == r = ipt
  | f ipt < r  = biSection f r (nIpt max) ipt max
  | f ipt > r  = biSection f r (nIpt min) min ipt
  where nIpt x = ((ipt + x) / 2)
