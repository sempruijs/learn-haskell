data Direction = North | South

data Shape = Circle Float | Rectangle Float Float deriving Show

surface :: Shape -> Float
surface (Circle r) = r * r * pi 
surface (Rectangle l1 l2) = l1 * l2

data TraficLight = Red | Orange | Green

instance Eq TraficLight where
  Red == Red = True
  Orange == Orange = True
  Green == Green = True
  _ == _ = False

instance Show TraficLight where
  show Red = "Red light"
  show Orange = "Orange light"
  show Green = "Green light"
