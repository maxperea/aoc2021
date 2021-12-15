-- module TEST where
import Data.Char(digitToInt)
import Data.Maybe(catMaybes)
import Data.List(transpose)
type X = Int
type Y = Int
type Risk = Int
type Distance = Int
type Node = (Risk, Distance)
type Map = [[Node]]
type Pos = (X, Y)

main = interact (show . solve 2.  parse)

solve x m =  (snd . head . head) sol  - (fst . head . head) sol
  where sol = (updateX x . replace (l,l) (start,start) . fullMap) m
        l = (length m * 5) - 1
        start = 9
 
parse :: String -> Map
parse =  map (map ((\i -> (i,9999)) . digitToInt)) . map (filter (/='\r')) . lines

getAdjacent :: Pos -> [Pos]
getAdjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)] 

getAdjacentNodes :: Pos -> Map -> [Node]
getAdjacentNodes p m = catMaybes $ map (getNode m) (getAdjacent p)

newNode :: Node -> [Node] -> Node
newNode (r, d) ls = (r, dn)
  where dn = (minimum . (++[d]) . map (+r) . snd . unzip) ls

get :: Pos -> Map -> Node
get (x,y) m = m !! y !! x

replace :: Pos -> a -> [[a]] -> [[a]]
replace (x,y) a m = take y m ++ [r] ++ drop (y+1) m
  where r = take x (m !! y) ++ [a] ++ drop (x+1) (m !! y)

getNode :: Map -> Pos -> Maybe Node
getNode m (x,y) | (x < 0) || (y < 0) || (x >= length (head m)) || (y >= length m) = Nothing
                | otherwise = Just $ m !! y !! x

update :: Pos -> Map -> Map
update p m = replace p nn m
  where nn = newNode (get p m) (getAdjacentNodes p m)

updateAll :: Map -> Map
updateAll m = updateAllAux ((length m) * (length (head m)) - 1) m
  where updateAllAux (-1) m = m
        updateAllAux n m = updateAllAux (n-1) (update (n `mod` length (head m), n `div` length m) m)

updateX :: Int -> Map -> Map
updateX x m = iterate updateAll m !! x

  -- BIGGER MAP
  
fullMap :: Map -> Map
fullMap = transpose . bigMap 4 . transpose . bigMap 4

bigMap :: Int -> Map -> Map
bigMap 0 m = m
bigMap n m = zipWith (++) (bigMap (n-1) m) (incrementMap n m)

incrementMap :: Int -> Map -> Map
incrementMap x = map (map (\(r,d) -> (((r+x-1) `mod` 9) + 1,d)))

