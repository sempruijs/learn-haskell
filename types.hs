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

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo String where
  yesno "" = False
  yesno s = s == "True"


instance YesNo Bool where
  yesno = id  

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
