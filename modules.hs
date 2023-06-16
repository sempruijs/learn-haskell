import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub 

spell :: String -> String
spell word = intersperse '.' word

shift :: String -> Int -> String
shift msg amount = map shiftChar msg
  where shiftChar c = chr $ ord c + amount   


encode msg = shift msg 10

decode msg = shift msg (-10)


alphabet = ['a'..'z'] ++ alphabet

alphabetIndex :: Char -> Int
alphabetIndex c = (ord $ toLower c) - 97

insertHarko :: Map.Map String String -> Map.Map String String
insertHarko = Map.insert "harko" "harko the best"

