data Shape = Circle Float | Rectangle Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle r) = r * pi ^ 2
surface (Rectangle w h) = w * h


data Person = Person {firstName :: String, lastName :: String, height :: Float} deriving (Show)
sem = Person "Sem" "Pruijs" 1.8

data Vector a = Vector a a a deriving (Show)

vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vPlus` (Vector d e f) = Vector (a + d) (b + e) (c + f)

-- maybe this could be implemented more compact with curling
vScaler :: (Num a) => a -> Vector a -> Vector a
vScaler s (Vector a b c) = Vector (s*a) (s*b) (s*c) 



