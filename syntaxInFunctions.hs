factorial 0 = 1
factorial n = n * factorial (n-1)

third (_,_,c) = c

length' [] = 0
length' (_:xs) = 1 + length' xs

shortsAdvice degrees
    | degrees <= 0 = "It's freezing absolutely not!"
    | degrees <= 20 = "Nha to cold"
    | degrees <= 26 = "Today is a good day for shorts"
    | degrees > 26 = "Go on, but do not forget the sunscreen."
