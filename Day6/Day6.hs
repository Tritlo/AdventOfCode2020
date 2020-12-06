module Main where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond as = case y of
                        [] -> [x]
                        (y:ys) -> x:(splitWhen cond ys)
  where (x,y) = break cond as

-- HoTT Girl Summer
getInput :: FilePath -> IO [[String]]
getInput = fmap (splitWhen (== "") . lines) . readFile

solution :: [String] -> Int
solution strs = Set.size $ Set.fromList $ concat strs

solution2 :: [String] -> Int
solution2 strs = Set.size $ foldl1 Set.intersection $ map Set.fromList strs
   where sorted = sort strs



main :: IO ()
main = do
          getInput "testInput" >>= print . sum . map solution
          getInput "input" >>= print . sum . map solution
          getInput "testInput" >>= print . map solution2
          getInput "input" >>= print . sum . map solution2