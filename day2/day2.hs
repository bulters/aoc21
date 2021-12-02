{-# LANGUAGE BlockArguments #-}
module Main where

import qualified Text.Parsec as Parsec
import Text.Parsec.String
import Data.Either

data Position = Pos Int Int Int deriving (Show)
data Direction = Up | Down | Forward deriving (Show)
data Movement = Move Direction Int deriving (Show)

parser :: Parsec.Parsec String () Movement
parser = do
  letters <- Parsec.many1 Parsec.letter
  Parsec.spaces
  digits <- read <$> Parsec.many1 Parsec.digit
  let d = case letters of 
            "forward" -> Forward
            "up" -> Up
            "down" -> Down
            _ -> Forward
  return (Move d digits)

applyMovement :: Position -> Movement -> Position
applyMovement (Pos x y z) (Move Up a) = Pos x (y - a) z
applyMovement (Pos x y z) (Move Down a) = Pos x (y + a) z
applyMovement (Pos x y z) (Move Forward a) = Pos (x + a) y z

applyMovementWithAim :: Position -> Movement -> Position
applyMovementWithAim (Pos x y z) (Move Up a) = Pos x y (z - a)
applyMovementWithAim (Pos x y z) (Move Down a) = Pos x y (z + a)
applyMovementWithAim (Pos x y z) (Move Forward a) = Pos (x + a) (y + (a *z)) z


multiplyPos :: Position -> Int
multiplyPos (Pos x y _) = x * y

startingPos = Pos 0 0 0

main :: IO ()
main = do
  input <- readFile "input"
  let ls = lines input
  let is = rights $ map (Parsec.parse parser "") ls

  let p1 = foldl applyMovement startingPos is
  print $ multiplyPos p1

  let p2 = foldl applyMovementWithAim startingPos is
  print $ multiplyPos p2
