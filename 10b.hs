import Data.List

main = interact (show . getMiddle . sort . filter (/= 0) . map (score . f []) . lines)

getMiddle :: [a] -> a
getMiddle s = head $ drop (length s `div` 2) s

f :: String -> String -> String
f (s:stack) (x:xs) | x == '(' = f (x:s:stack) xs
                   | x == '[' = f (x:s:stack) xs
                   | x == '<' = f (x:s:stack) xs
                   | x == '{' = f (x:s:stack) xs
                   | x == ')' && s == '(' = f stack xs
                   | x == ']' && s == '[' = f stack xs
                   | x == '>' && s == '<' = f stack xs
                   | x == '}' && s == '{' = f stack xs
                   | x == ')' = []
                   | x == ']' = []
                   | x == '}' = []
                   | x == '>' = []
f [] (x:xs) = f [x] xs
f stack [] = stack

score :: String -> Int
score s = scoreAux (length s) $ reverse s

scoreAux :: Int -> String -> Int
scoreAux 0 _ = 0
scoreAux n (x:xs) = points x + 5 * scoreAux (n-1) xs
scoreAux _ [] = 0

points :: Char -> Int
points '(' = 1
points '[' = 2
points '{' = 3
points '<' = 4
