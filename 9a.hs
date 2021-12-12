import Data.List
import Data.List.Split
type World = [[Int]]
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
solve w = sum [1 + (w !! y !! x) | x <- [0..length (head w) - 1], y <- [0..length w - 1], lowPoint w x y]
