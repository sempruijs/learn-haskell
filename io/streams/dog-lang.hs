main = do
  contents <- getContents
  putStr $ unlines . map filterWaf $ lines contents

filterWaf :: String -> String
filterWaf s = unwords . filter (== "waf") $ words s
