data Direction = North | South

data Shape = Circle Float | Rectangle Float Float deriving Show

surface :: Shape -> Float
surface (Circle r) = r * r * pi 
surface (Rectangle l1 l2) = l1 * l2
