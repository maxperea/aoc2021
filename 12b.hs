import Data.List
import Data.Char
type Room = String
type Road = (Room, Room)

main = interact (show . length . filter (not . null) .(\x -> traverse x "start") . map parse . lines)

small :: Room -> Bool
small = not . isUpper . head

parse :: String -> Road
parse = parseAux 0
  where parseAux n s | s !! n == '-' = (take n s, drop (n+1) s) 
                     | otherwise = parseAux (n+1) s

getConnected :: Room -> [Road] -> [Room]
getConnected s rs = map snd (filter ((== s) . fst ) rs) ++ map fst (filter ((== s) . snd) rs)

traverse :: [Road] -> Room -> [[Room]]
traverse = traverseAux [] True

traverseAux :: [Room] -> Bool -> [Road] -> Room -> [[Room]]
traverseAux visited x rs s  | s == "end" = [s:visited] 
                            | s == "start" && (not . null) visited = [[]]
                            | (small s) && (s `elem` visited) && not x = [[]]
                            | (small s) && (s `elem` visited) && x = foldl1 (++) $ map (traverseAux (s:visited) False rs) (getConnected s rs) 
                            | otherwise = foldl1 (++) $ map (traverseAux (s:visited) x rs) (getConnected s rs) 
