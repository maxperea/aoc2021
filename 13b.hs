import Data.List.Split
type Paper = [[Bool]]
type Coord = (Int, Int)

main = interact (show . folds . parsePaper . map (readPair . splitOn ",") . head . splitOn [""] . lines)

folds = foldY . foldY . foldY . foldX . foldY . foldX . foldY . foldX . foldY . foldX . foldY . foldX

readPair :: [String] -> (Int, Int)
readPair (x:y:[]) = (read x, read y)
readPair _ = (0,0)

largestCoord :: [Coord] -> Coord
largestCoord = (\(a, b) -> (foldl1 max a, foldl1 max b) ) . unzip

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

foldX :: Paper -> Paper
foldX p = merge leftHalf rightHalf
  where rightHalf = map (reverse . drop (1 + length (head p) `div` 2)) p
        leftHalf = map (take (length (head p) `div` 2)) p

foldY :: Paper -> Paper
foldY p = merge upperHalf (reverse lowerHalf)
  where upperHalf = take (length p `div` 2) p
        lowerHalf = drop (1 + length p `div` 2) p

count :: Paper -> Int
count = sum . map sum . map (map fromEnum )
