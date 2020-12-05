module Main where

import Data.Bits
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

getInput :: FilePath -> IO [String]
getInput = fmap lines . readFile

-- FBFBBFF RLR
-- 0101100 =  = 44
-- RLR = 101 = 4 + 1 = 5

toBits :: String -> Int
toBits str = toInt str
  where toInt :: String -> Int
        toInt str = foldl f 0 $ zip [(0 :: Int)..] (reverse str)
          where f b (i, 'B')  = setBit b i
                f b (i, 'R') =  setBit b i
                f b _ = id b

main :: IO ()
main = do
     getInput "testInput" >>= print
     getInput "testInput" >>= print . map toBits
     input <- getInput "input"
     let seatIds = map toBits input
         (mi, ma) = (minimum seatIds, maximum seatIds)
     print $ (Set.fromList [mi..ma]) Set.\\ (Set.fromList seatIds)
