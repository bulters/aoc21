{-# LANGUAGE LambdaCase #-}

import Data.List (transpose)
import Data.Either (partitionEithers)

data Bit = Zero | One deriving (Show)
type BitArray = [Bit]

parseBitstring :: String -> BitArray
parseBitstring = map $ \case
  '0' -> Zero
  '1' -> One
  c -> error $ "Unknown input: " ++ [c]
 
flipBit :: Bit -> Bit
flipBit One = Zero
flipBit Zero = One

flipBitArray :: BitArray -> BitArray
flipBitArray = map flipBit

bitHisto :: BitArray -> (Int, Int)
bitHisto ba = foldl (\(z,o) -> \case
  Zero -> (z+1, o)
  One  -> (z, o+1)) (0,0) ba

-- most frequent bit
mfb :: BitArray -> Bit
mfb bits = let histo = bitHisto bits
               zlto = (fst histo) > (snd histo)
           in case zlto of
             True  -> Zero
             False -> One
             
-- left or right, Zero gives left/first, One gives right/second
lor :: (x, x) -> Bit -> x
lor (x, _) Zero = x
lor (_, x) One  = x

toInt :: BitArray -> Int
toInt x = foldl (\a b -> lor (0, 1) b + 2 * a) 0 x

partitionByBitAt :: Int -> [BitArray] -> ([BitArray], [BitArray])
partitionByBitAt i =  partitionEithers . map binaryPartition
  where
    binaryPartition n = 
      case n !! i of 
        Zero -> Left n
        One -> Right n
   

filterBitsByFreq :: (Ordering -> Bit -> [BitArray] -> Int -> [BitArray])
filterBitsByFreq c whenEqual counts i =
  let (z, o) = partitionByBitAt i counts
  in case compare (length z) (length o) of
    EQ -> lor (z, o) whenEqual
    r -> if r == c then z else o


filterOn :: (Ordering -> Bit -> [BitArray] -> [Int] -> [BitArray])
filterOn c whenEqual xs ys = concat $ filter (\x -> length x == 1) $ scanl (filterBitsByFreq c whenEqual) xs ys

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let bits = map parseBitstring input
      perSB = transpose bits
      gamma = map mfb perSB
      epsilon = flipBitArray gamma
  print $ toInt gamma * toInt epsilon
  let lim = length (head bits) - 1
      idxs = [0..lim]
      oxy =  head $ filterOn GT One bits idxs
      co2 =  head $ filterOn LT Zero bits idxs
  print $ toInt oxy * toInt co2
  
