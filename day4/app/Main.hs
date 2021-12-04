{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (transpose, find, findIndex)
import Data.List.Split
import Data.Maybe (fromJust)

type Board = ([[(Int, Bool)]], Int)

intoBoard :: [[Int]] -> Board
intoBoard l = (map (map (, False)) l, 9999)

markBoard ::  Board -> Int -> Board
markBoard (b, m) i = (map (map (\(n, v) -> if n == i then (n, True) else (n, v))) b, i)

rowBingo :: [(x, Bool)] -> Bool
rowBingo = all snd

hasBingo :: Board -> Bool
hasBingo (b, _) = any rowBingo b || any rowBingo (transpose b)

unmarkedNumbers :: Board -> [Int]
unmarkedNumbers (b, _) = map fst $ filter (not . snd) $ concat b

main :: IO ()
main = do
  (ns:bs) <- splitOn [""] . lines <$> readFile "input"
  let nums = concatMap (map read . splitOn ",") ns :: [Int]
      boards = map intoBoard (map (map (map read .filter (/= "") . splitOn " ")) bs :: [[[Int]]])
      allBoards = scanl (\bs n -> map (`markBoard` n) bs) boards nums

  let firstWin = fromJust $ find (any hasBingo) allBoards
      firstWinningBoard = fromJust $ find hasBingo firstWin
      firstUnmarkedNumbers = unmarkedNumbers firstWinningBoard
  print $ sum firstUnmarkedNumbers * snd firstWinningBoard

  let lastNotWinIdx = fromJust $ findIndex (not. all hasBingo) (reverse allBoards)
      lastNotWinBoardIdx = fromJust $ findIndex (not . hasBingo) (reverse allBoards !! lastNotWinIdx)
      lastWinningBoard = (reverse allBoards !! ( lastNotWinIdx - 1 )) !! lastNotWinBoardIdx
      lastUnmarkedNumbers = unmarkedNumbers lastWinningBoard
  print $ sum lastUnmarkedNumbers * snd lastWinningBoard

