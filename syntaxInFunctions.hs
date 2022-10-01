factorial 0 = 1
factorial n = n * factorial (n-1)

third (_,_,c) = c

length' [] = 0
length' (_:xs) = 1 + length' xs

shortsAdvice degrees
    | degrees <= freezing = "It's freezing absolutely not!"
    | degrees <= cold = "Nha too cold"
    | degrees <= good = "Today is a good day for shorts"
    | otherwise     = "Go on, but do not forget the sunscreen."
    where (freezing, cold, good) = (0, 20, 26)




