{-# LANGUAGE TupleSections #-}
module Main where

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Data.Maybe

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Debug.Trace


getInput :: FilePath -> IO ((IntSet, Int), (IntSet, Int), Int)
getInput = fmap (\i -> (fromLines 'L' i, fromLines '#' i, length $ lines i)) . readFile
  where fromLine :: Char -> (Int, String) -> [Int]
        fromLine c (i, ln)  = map (\(j,_) -> toLineLn (length ln) (j,i)) $ filter ((==) c . snd) $ zip [0..] ln
        fromLines :: Char -> String -> (IntSet, Int)
        fromLines c = (\xs -> (Set.fromList $ concatMap (fromLine c) $ zip [0..] xs, length $ head xs)) . lines

fromLineLn :: Int -> Int -> (Int, Int)
fromLineLn lineLn i = (i `div` factor, i `mod` factor)
  where factor = factorFun lineLn

factorFun :: Int -> Int
factorFun lineLn = lineLn+1

toLineLn  :: Int -> (Int, Int) -> Int
toLineLn lineLn (j,i) = (factorFun lineLn)*j + i

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

solution1 :: IntSet -> Int -> Int
solution1 seating lineLn  = Set.size $ fixEQ (update lineLn seating) seating

seen :: IntMap [[Int]] -> IntSet -> IntSet -> Int -> Int
seen diags empty occupied inp = sum $ map firstSeen dirs
   where dirs = diags IM.! inp
         firstSeen :: [Int] -> Int
         firstSeen [] = 0
         firstSeen (loc:locs) | loc `Set.member` occupied = 1 -- isOccupied
                              | loc `Set.member` empty = 0 -- isEmpty
                              | otherwise = firstSeen locs

computeDiags :: Int -> Int -> IntSet -> Int -> [[Int]]
computeDiags lineLn numLines floor inp =
  map (filter (`Set.notMember` floor))
   [nw, se, ne, sw, w, n, e, s]
  where  (x,y) = fromLineLn lineLn inp
         nw = map (toLineLn lineLn) $ zipWith (\a b -> (x-a,y-b)) [1..x] [1..y]
         se = map (toLineLn lineLn) $ zipWith (\a b -> (x+a,y+b)) [1..lineLn-x] [1..numLines-y]
         ne = map (toLineLn lineLn) $ zipWith (\a b -> (x+a,y-b)) [1..lineLn-x] [1..y]
         sw = map (toLineLn lineLn) $ zipWith (\a b -> (x-a,y+b)) [1..x] [1..numLines-y]
         w =  map (toLineLn lineLn. \a -> (x-a,y)) [1..x]
         n =  map (toLineLn lineLn. \a -> (x,y-a)) [1..y]
         e =  map (toLineLn lineLn. \a -> (x+a,y)) [1..lineLn-x]
         s =  map (toLineLn lineLn. \a -> (x,y+a)) [1..numLines-y]

update2 :: IntSet -> IntMap [[Int]] -> IntSet -> IntSet
update2 seating diags occupied = res
  where willBeOccupied :: Int -> Bool
        willBeOccupied loc = if loc `Set.notMember` occupied
                             then occ == 0 else occ < 5
         where occ = seen diags empty occupied loc
        res = Set.filter willBeOccupied seating
        empty = seating `Set.difference` occupied

solution2 :: IntSet -> Int -> Int -> Int
solution2 seating lineLn numLines =
        Set.size $ fixEQ (update2 seating diags) Set.empty
  where diags = IM.fromList $ map (\i -> (i, computeDiags lineLn numLines floor i)) $ Set.elems seating
        floor = Set.fromAscList (map (toLineLn lineLn) $
                 (\x y -> (x,y)) <$> [0..lineLn] <*> [0..numLines])
                 `Set.difference` seating


main :: IO ()
main = do -- putStrLn "ti-seen-1"
          --  ((emptySeats, lineLn), (occupied, _), numLines) <- getInput "ti-seen-1"
          --  print (map (fromLineLn lineLn) $ Set.toList emptySeats)
          --  print (seen lineLn numLines emptySeats occupied (toLineLn lineLn (3,4)))

          --  putStrLn "ti-seen-2"
          --  ((emptySeats, lineLn), (occupied, _), numLines) <- getInput "ti-seen-2"
          --  print (map (fromLineLn lineLn) $ Set.toList emptySeats)
          --  print (seen lineLn numLines emptySeats occupied (toLineLn lineLn (1,1)))

          --  putStrLn "ti-seen-3"
          --  ((emptySeats, lineLn), (occupied, _), numLines) <- getInput "ti-seen-3"
          --  print (map (fromLineLn lineLn) $ Set.toList emptySeats)
          --  print (seen lineLn numLines emptySeats occupied (toLineLn lineLn (3,3)))

          --  ((seating, lineLn), (occupied, _), numLines) <- getInput "test-input"
          --  print (solution1 seating lineLn)
          --  print (solution2 seating lineLn numLines)

           ((seating, lineLn), (_, _), numLines) <- getInput "input"
          --  print (solution1 seating lineLn)
           print (solution2 seating lineLn numLines)

