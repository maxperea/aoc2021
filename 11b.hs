import Data.List
import Data.List.Split
type Octopus = (Int, Bool, Int)
type OctoGrid = [[Octopus]]
type OctoRow = [Octopus]
type OctoPos = (Int,Int)

main = interact (show . step 0. f . lines)

third (a,b,c) = c
second (a,b,c) = b

untilFixPoint :: OctoGrid -> OctoGrid
untilFixPoint og =
  if flashGrid og == og then og else untilFixPoint $ flashGrid og

step :: Int -> OctoGrid -> Int
step n og | allBlink og = n
step n og = step (n + 1) $ (untilFixPoint . refreshGrid) og


allBlink :: OctoGrid -> Bool
allBlink og = all second (foldl1 (++) og)


f :: [String] -> OctoGrid
f = map (map (\x -> ((read :: String -> Int) x, False, 0)) . filter (not . null) . splitOn "")

refreshGrid :: OctoGrid -> OctoGrid
refreshGrid og = refreshGridAux og (length og - 1) (length og - 1) (length og - 1)

refreshGridAux :: OctoGrid -> Int -> Int -> Int -> OctoGrid
refreshGridAux og 0 0 size = refresh (0,0) og
refreshGridAux og n 0 size = refresh (n, 0) $ refreshGridAux og (n-1) size size
refreshGridAux og n m size = refresh (n, m) $ refreshGridAux og n (m-1) size

refresh :: OctoPos -> OctoGrid -> OctoGrid
refresh (x,y) og = take y og ++ [take x (og !! y) ++ [refreshOcto (og !! y !! x)] ++ drop (x+1) (og !! y)] ++ drop (y+1) og

refreshOcto :: Octopus -> Octopus
refreshOcto (x, True, f) = (1, False, f)
refreshOcto (x, False, f) = (x+1, False, f)


flashGrid :: OctoGrid -> OctoGrid
flashGrid og = flashGridAux og (length og - 1) (length og - 1) (length og - 1)

flashGridAux :: OctoGrid -> Int -> Int -> Int -> OctoGrid
flashGridAux og 0 0 size = flashIfFull (0,0) og
flashGridAux og n 0 size = flashIfFull (n, 0) $ flashGridAux og (n-1) size size
flashGridAux og n m size = flashIfFull (n, m) $ flashGridAux og n (m-1) size

flashIfFull :: OctoPos -> OctoGrid -> OctoGrid
flashIfFull (x,y) og | shouldFlash (og !! y !! x) = flash (x,y) og
                     | otherwise = og

flash :: OctoPos -> OctoGrid -> OctoGrid
flash (x,y) og | y == 0 =  [flashMiddleRow x (og !! y)] ++ [flashRow x (og !! (y+1))] ++ drop (y+2) og
               | y +1 == length og =    take (y-1) og ++       [flashRow x (og !! (y-1))] ++ [flashMiddleRow x (og !! y)]
               | otherwise = take (y-1) og ++       [flashRow x (og !! (y-1))] ++
                                        [flashMiddleRow x (og !! y)] ++
                                        [flashRow x (og !! (y+1))] ++
                                        drop (y+2) og

flashRow :: Int -> OctoRow -> OctoRow
flashRow x row          | x == 0 = [octoIncrease (head row)] ++ [octoIncrease (row !! (x+ 1))] ++ drop (x+2) row
                        | x +1 == length row  = take (x-1) row ++ [octoIncrease (row !! (x -1))] ++ [octoIncrease (row !! x)]
                        | otherwise = take (x-1) row ++ [octoIncrease (row !! (x-1))]
                                ++ [octoIncrease (row !! x)]
                                ++ [octoIncrease (row!! (x+1))]
                                ++ drop (x+2) row

flashMiddleRow :: Int -> OctoRow -> OctoRow
flashMiddleRow x row | x == 0 = [octoFlash (head row)] ++ [octoIncrease (row !! (x+ 1))] ++ drop (x+2) row
                     | x + 1 == length row = take (x-1) row ++ [octoIncrease (row !! (x -1))] ++ [octoFlash (row !! x)]
                     | otherwise = take (x-1) row    ++ [octoIncrease (row !! (x-1))]
                                         ++ [octoFlash (row !! x)]
                                         ++ [octoIncrease (row!! (x+1))]
                                         ++ drop (x+2) row


octoIncrease :: Octopus -> Octopus
octoIncrease (x, b, f) = (x+1, b, f)

octoFlash :: Octopus -> Octopus
octoFlash (x, b, f) = (x, True, f+1)

shouldFlash :: Octopus -> Bool
shouldFlash (_, True, _) = False
shouldFlash (x, False, _) | x > 9 = True
                       | otherwise = False
