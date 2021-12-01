module Main where

import Data.List (tails)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = zip xs (tail xs)

increases = \x -> (snd x) > (fst x)

window m = foldr (zipWith (:)) (repeat []) . take m . tails

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

part1 :: [Int] -> Int
part1 ls = count True $ map increases $ pairs ls

part2 :: [Int] -> Int
part2 ls = count True $ map increases $ pairs $ map sum $ window 3 ls

main :: IO ()
main = do
  input <- readFile "input"
  let ls = map read $ lines input :: [Int]
  print $ part1 ls
  print $ part2 ls
