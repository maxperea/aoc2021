import Data.List
import Data.Function
import Data.Map (fromListWith, toList, Map)
import Data.List.Split
import ByteCodeAsm (ssElts)

type Template = (String, String)
type Count = [(String, Int)]
type Puzzle = (Count, [Template])

main = interact (show . solve . parse)

multiStep s = iterate step s !! 40

solve = getScore . map (sum . map snd) . groupBy ((==) `on` fst). sort . map (\(x,y) -> (head (tail x), y)) . fst . multiStep

getScore s = maximum s - minimum s

readPair :: [String] -> (String, String)
readPair [x, y] = (x, y)
readPair _ = ("","")

parse s = ((sToCount . head . head) a, (map (readPair . splitOn " -> ") . head . tail) a )
  where a = (splitOn [""] . lines) s

sToCount :: String -> Count
sToCount = sToCountAux []

sToCountAux :: Count -> String -> Count
sToCountAux c [] = c
sToCountAux c [x] = c
sToCountAux c (x:y:xs) = sToCountAux (addToCount 1 [x,y] c) (y:xs)

addToCount :: Int-> String -> Count -> Count
addToCount n s [] = [(s,n)]
addToCount n s ((r,c):cs) | s == r = (r, c+n):cs
                        | otherwise = (r,c) : addToCount n s cs

decreaseCount :: Int -> String -> Count -> Count
decreaseCount n s [] = [(s,0)]
decreaseCount n s ((r,c):cs) | s == r = (r, c-n):cs
                        | otherwise = (r,c) : decreaseCount n s cs

update :: Count -> Count -> [Template] -> Count
update c c2 [] = filter ((>0) . snd) $ c2 ++ c
update c c2 ((s1, s2):ss) | s1 `elem` map fst c = update (decreaseCount n s1 c) ((addToCount n (head s1 : s2) . addToCount n (s2 ++ tail s1 )) c2) ss
                       | otherwise = update c c2 ss
                       where n = getCount s1 c

getCount :: String -> Count -> Int
getCount s = snd . head . filter ((==s) . fst)

step :: Puzzle -> Puzzle
step (c, ts) = (update c [] ts, ts)
