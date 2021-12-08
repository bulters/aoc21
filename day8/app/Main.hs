module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn, unzip5, unzip6, unzip4)
import Control.Arrow ((&&&))
import Data.Digits (unDigits)

parseLine :: String -> [[S.Set Char]]
parseLine s =  map (map S.fromList . words) (splitOn " | " s)

findWiring :: [S.Set Char] -> S.Set Char -> (Bool, Bool)
findWiring ds =
  let isBaseDigit = \x -> length x `elem` [2,3,4,7]
      -- 1:   c  f  Derive by count
      -- 7: a c  f  Derive by count
      -- 4:  bcd f  Derive by count
      -- 8: abcdefg Derive by count
      [is1, is7, is4, is8] = sortOn length $ filter isBaseDigit ds
      -- b and d are only used by set 4 - set 1
      bd = is4 S.\\ is1
      -- e and g are only used by the set of all wires - set 1 + set 4 + set 7 
      eg = is8 S.\\ foldl1 S.union [is1, is4, is7]
  -- NEW TRICK: Use Arrow &&&: Send input (pointless function arg)
  -- into both functions and combine the results, resulting in a 
  -- S.Set Char -> (Bool, Bool) return type.
  in (S.isSubsetOf bd &&& S.isSubsetOf eg)

deduce :: (S.Set Char -> (Bool, Bool)) -> S.Set Char -> Int
deduce dps digit = deduce' (length digit)
  where
    deduce' 5 = case dps digit of
      -- is there is any overlap with the eg set
      -- 5: ab d fg
      (True, _) ->  5
      -- if there is any overlap with the bd set
      -- 2: a cde g
      (_, True) ->  2
      -- otherwise, it's a 3
      -- 3: a cd fg
      _         ->  3
    deduce' 6 = case dps digit of
      -- If there is overlap with the eb and bd set
      -- 6: ab defg
      (True, True)  ->  6
      -- If there is only overlap with the bd set
      -- 9: abcd fg
      (True, False) ->  9
      -- Otherwise, it's a zero
      -- 0: abc efg
      _             ->  0
    deduce' l = case l of
      2 -> 1
      3 -> 7
      4 -> 4
      _ -> 8

deduceLine :: [[S.Set Char]] -> Int
deduceLine line = let wirings = head line
                      digits = line !! 1
                      w = findWiring wirings
                  in unDigits 10 $ map (deduce w) digits
main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let signals = map parseLine input
  let displays = map (!! 1) signals
      lengths = map (map S.size)  displays
      correctLengths = map (filter (`elem` [2,3,4,7])) lengths
      lengthCounts = map length correctLengths
  print $ sum lengthCounts
  let sumDigits = sum $ map deduceLine signals
  print sumDigits



