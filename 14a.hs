import Data.List
import Data.Map (fromListWith, toList)
import Data.List.Split
type Template = (String, String)
type Puzzle = (String, [Template])
main = interact (show . solve . parse)

solve s = count $ fst $ iterate step s !! 12

readPair :: [String] -> (String, String)
readPair [x, y] = (x, y)
readPair _ = ("","")

parse s = ((head . head) a, (map (readPair . splitOn " -> ") . head . tail) a )
  where a = (splitOn [""] . lines) s

step :: Puzzle -> Puzzle
step p = stepAux [] p


stepAux :: String -> Puzzle -> Puzzle
stepAux p ([a, b], l)  = (p ++ (update [a,b] l) ++ [b], l)
stepAux p (a:b:abs, l) = stepAux (p ++ update [a,b] l) (b:abs, l)

update :: String -> [Template] -> String
update s [] = s
update s ((x, y):xys) | s == x = take 1 s ++ y
                      | otherwise = update s xys


count :: String -> Int
count s = ((maximum . snd . unzip . frequency) s) - ((minimum . snd . unzip . frequency) s)

frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])
