{-# LANGUAGE TupleSections #-}
module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set


getInput :: FilePath -> IO (IntSet, Int)
getInput = fmap  fromLines . readFile
  where fromLine :: (Int, String) -> [Int]
        fromLine (i, ln)  = map (\(j,_) -> toLineLn (length ln) (j,i)) $ filter ((==) 'L' . snd) $ zip [0..] ln
        fromLines :: String -> (IntSet, Int)
        fromLines  = (\xs -> (Set.fromList $ concatMap fromLine $ zip [0..] xs, length $ head xs)) . lines

fromLineLn :: Int -> Int -> (Int, Int)
fromLineLn lineLn i = (i `div` lineLn,i `mod` lineLn)

toLineLn  :: Int -> (Int, Int) -> Int
toLineLn lineLn (j,i) = lineLn*j + i

neighbors :: Int -> Int -> IntSet
neighbors lineLn inp = Set.delete inp $
                      Set.fromDistinctAscList $
                        (\a b -> toLineLn lineLn (x+a, y+b)) <$> [-1,0,1] <*> [-1,0,1]
   where (x,y) = fromLineLn lineLn inp

update :: Int -> IntSet -> IntSet -> IntSet
update lineLn seating occupied = Set.filter willBeOccupied seating
  where willBeOccupied :: Int -> Bool
        willBeOccupied loc = if loc `Set.notMember` occupied
                             then Set.null occupiedNeighbors
                             else numOccupiedNeighbors < 4
         where validNeighbors = neighbors lineLn loc
               numOccupiedNeighbors = Set.size occupiedNeighbors
               occupiedNeighbors = validNeighbors `Set.intersection` occupied

fixEQ :: Eq a => (a -> a) -> a -> a
fixEQ f a | a == fa = a
          | otherwise = fixEQ f fa
  where fa = f a

main :: IO ()
main = do (seating, lineLn) <- getInput "input"
          print (Set.size $ fixEQ (update lineLn seating) seating)

