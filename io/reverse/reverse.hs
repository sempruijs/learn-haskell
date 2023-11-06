main = do
  print "Enter a sentence"
  s <- getLine
  if null s 
    then return ()
    else do
      print $ reverse s
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
