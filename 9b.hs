import Data.List
import Data.List.Split
type World = [[Int]]
type Point = (Int, Int)
type Basin = [Point]

main = interact (show . solve .map f . lines)

f = map (read :: String -> Int) . filter (\x -> length x == 1) . splitOn ""

lowPointRow :: [Int] -> Int -> Bool
lowPointRow row x
  | x == 0 = head row < row !! 1
  | (x + 1) == length row = row !! x < row !! (x -1)
  |otherwise = row !! (x-1) > row !! x && row !! (x+1) > row !! x

lowPoint :: World -> Int -> Int -> Bool
lowPoint w x y  = lowPointRow (w !! y) x && lowPointRow (transpose w !! x) y

solve :: World -> Int
solve w = product $ take 3 $ reverse $ sort [length (nub (getBasin w (x,y))) | x <- [0..length (head w) - 1], y <- [0..length w - 1], lowPoint w x y]

solve2 w = map ((getBasin w)) [(x,y) | x <- [0..length (head w) - 1], y <- [0..length w - 1], lowPoint w x y]



getLevel :: (Int, Int) -> [[a]] -> a
getLevel p w = w !! snd p !! fst p

getBasin w p = getBasinAux w (Just p) p []

getBasinAux :: World -> Maybe Point -> Point -> Basin -> Basin
getBasinAux w Nothing last b = b
getBasinAux w (Just p) last b | getLevel p w == 9 = b
                              | (getLevel last w) > (getLevel p w) = b
                              | p `elem` b = b
                              | otherwise = getBasinAux w (upper p w) p (p:b) ++ getBasinAux w (lower p w) p (p:b) ++ getBasinAux w (right p w) p (p:b) ++ getBasinAux w (left p w) p (p:b)

upper :: Point -> World -> Maybe Point
upper (x,y) w | y == 0 = Nothing
              | otherwise = Just (x, y-1)

lower :: Point -> World -> Maybe Point
lower (x,y) w | y == (length w - 1) = Nothing
              | otherwise = Just (x, y+1)

left :: Point -> World -> Maybe Point
left (x,y) w | x == 0 = Nothing
             | otherwise = Just (x-1, y)

right :: Point -> World -> Maybe Point
right (x,y) w | x == (length (head w) - 1) = Nothing
              | otherwise = Just (x + 1, y)
