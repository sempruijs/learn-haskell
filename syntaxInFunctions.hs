factorial 0 = 1
factorial n = n * factorial (n-1)

third (_,_,c) = c

length' [] = 0
length' (_:xs) = 1 + length' xs