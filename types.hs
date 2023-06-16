data Shape = Circle Float | Rectangle Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle r) = r * pi ^ 2
surface (Rectangle w h) = w * h


data Person = Person {firstName :: String, lastName :: String, height :: Float} deriving (Show)
sem = Person "Sem" "Pruijs" 1.8
