
main = interact (show . sum . map g . lines)

g :: String -> Int
g = f [] 0

f :: String -> Int -> String -> Int
f (s:stack) score (x:xs) | x == '(' = f (x:s:stack) score xs
                         | x == '[' = f (x:s:stack) score xs
                         | x == '<' = f (x:s:stack) score xs
                         | x == '{' = f (x:s:stack) score xs
                         | x == ')' && s == '(' = f stack score xs
                         | x == ']' && s == '[' = f stack score xs
                         | x == '>' && s == '<' = f stack score xs
                         | x == '}' && s == '{' = f stack score xs
                         | x == ')' = 3
                         | x == ']' = 57
                         | x == '}' = 1197
                         | x == '>' = 25137
f [] score (x:xs) = f [x] score xs
f _ _ [] = 0
