module Main where
import Math.Geometry.Grid.Square
import Math.Geometry.Grid
import Data.List (sortOn)
import Data.Ord

getXY :: [[Int]] -> (Int, Int) -> Int
getXY g (x,y) = g !! x !! y

readInput :: String -> IO [[Int]]
readInput f = map (map (read . (: []))) . lines <$> readFile f

isLowest :: RectSquareGrid -> [[Int]] -> (Int, Int) -> Bool
isLowest g gvs p  = let pv = getXY gvs p
                        nbs = neighbours g p
                in all ((pv <) . getXY gvs) nbs

findLows :: RectSquareGrid -> [[Int]] -> [(Int, Int)]
findLows g gvs = filter (isLowest g gvs) $ indices g

mkGrid :: [[Int]] -> RectSquareGrid
mkGrid g = rectSquareGrid (length g) (length $ head g)

expandBasin :: RectSquareGrid -> [[Int]] -> [(Int, Int)] -> (Int, Int)-> [(Int, Int)]
expandBasin g gvs acc pos = if getXY gvs pos < 9 && notElem pos acc
                            then foldl (expandBasin g gvs) (acc ++ [pos]) (neighbours g pos)
                            else acc

main :: IO ()
main = do
  input <- readInput "input"
  let grid = mkGrid input
      lows = findLows grid input
  print $ sum $ map ((1+).getXY input) lows
  let top3 = take 3 $ sortOn Down $ map (length . expandBasin grid input []) lows
  print $ product top3
