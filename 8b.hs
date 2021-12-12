import AOC
import Data.List.Split
import Data.List

type Key = [(Char, String)]

main = interact (show . sum .map f. lines)

toList = splitOn " "

bunch xs = (take 10 xs, drop 11 xs)

f = solve . bunch . toList

solve :: ([String], [String]) -> Int
solve s = applyKey ((getKey . fst) s) $ snd s

applyKey :: Key -> [String] -> Int
applyKey k s = read $ foldl1 (++) $ map (applyKeyOnce k) s

applyKeyOnce :: Key -> String -> String
applyKeyOnce k s = fromSegment $ map (transL k) s

transL ::Key -> Char -> Char
transL k c = head [f | (f, s)  <- k, head s == c]

fromSegment :: String -> String
fromSegment s = fS $ sort s
  where fS "abcefg" = "0"
        fS "cf" = "1"
        fS "acdeg" = "2"
        fS "acdfg" = "3"
        fS "bcdf" = "4"
        fS "abdfg" = "5"
        fS "abdefg" = "6"
        fS "acf" = "7"
        fS "abcdefg" = "8"
        fS "abcdfg" = "9"
        fS x = "77777"

getKey :: [String] -> Key
getKey s =  seventhRule s $ sixthRule s $ fifthRule s $ fourthRule s $ thirdRule s $ firstRule s ++ secondRule s

get :: [String] -> String
get result = head (filter (\x -> length x == 1) result)

toS :: Key -> String
toS k = foldl1 (++) $ map snd k

firstRule :: [String] -> Key
firstRule s = [('a',head (seven s) \\ head (one s))]

secondRule :: [String] -> Key
secondRule s = [('g',get result)]
  where result = map (\\ mask) (zeroSixNine s)
        mask = head (four s) `union` head (seven s)

thirdRule :: [String] -> Key -> Key
thirdRule s k = ('d',get result) : k
  where result = map (\\ mask) (twoThreeFive s)
        mask = head (one s) `union` toS k

fourthRule :: [String] -> Key -> Key
fourthRule s k = ('b',get result) : k
  where result = map (\\ mask) (zeroSixNine s)
        mask = head (one s) `union` toS k

fifthRule :: [String] -> Key -> Key
fifthRule s k = ('f',get result) : k
  where result = map (\\ mask) (twoThreeFive s)
        mask = toS k

sixthRule :: [String] -> Key -> Key
sixthRule s k = ('c',get result) : k
  where result = map (\\ mask) (twoThreeFive s)
        mask = toS k

seventhRule :: [String] -> Key -> Key
seventhRule s k = ('e',get result) : k
  where result = map (\\ mask) (twoThreeFive s)
        mask = toS k


one s = [c | c <- s, length c == 2]
four s = [c | c <- s, length c == 4]
seven s = [c | c <- s, length c == 3]
eight s = [c | c <- s, length c == 7]
twoThreeFive s = [c | c <- s, length c == 5]
zeroSixNine s = [c | c <- s, length c == 6]
