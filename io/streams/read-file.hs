import System.IO;

main = do
  contents <- readFile "leo.txt"
  putStr contents