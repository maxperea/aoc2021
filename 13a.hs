import Data.List
import Data.List.Split
import Data.Bifunctor
type Paper = [[Bool]]
type Coord = (Int, Int)

main = interact (show . count . foldX . parsePaper . map (readPair . splitOn ",") . head . splitOn [""] . lines)

readPair :: [String] -> (Int, Int)
readPair [x, y] = (read x, read y)
readPair _ = (0,0)

largestCoord :: [Coord] -> Coord
largestCoord = bimap maximum maximum . unzip

emptyPaper :: Coord -> Paper
emptyPaper (x, y) = [[False | _ <- [0..x]] | _ <- [0..y+1]]

setCell :: Coord -> Bool -> Paper -> Paper
setCell (x,y) b p = take y p ++ newRow ++ drop (y+1) p
  where newRow = [take x oldRow ++ [b] ++ drop (x+1) oldRow]
        oldRow = p !! y

parsePaper :: [Coord] -> Paper
parsePaper ls = parsePaperAux (emptyPaper (largestCoord ls)) ls
  where
    parsePaperAux p [] = p
    parsePaperAux p (l:ls) = parsePaperAux (setCell l True p) ls

merge :: Paper -> Paper -> Paper
merge = zipWith (zipWith (||))

foldY :: Paper -> Paper
foldY p = merge upperHalf (reverse lowerHalf)
  where upperHalf = take (length p `div` 2) p
        lowerHalf = drop (1 + length p `div` 2) p

foldX :: Paper -> Paper
foldX = transpose . foldY . transpose

count :: Paper -> Int
count = sum . map fromEnum . concat
