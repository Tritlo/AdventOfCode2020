module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

testInput :: [Int]
testInput = [ 1721
            , 979
            , 366
            , 299
            , 675
            , 1456 ]

-- Part 1
-- x+y = 2020
-- y = 2020-x
-- x*y = x*(2020-x)= -x^2 + 2020x

f :: Set Int -> Int -> Maybe Int
f input x = if x `Set.member` input
            then Just (x*(2020-x))
            else Nothing

solution :: [Int] -> Maybe Int
solution input = loop input
  where modInput = Set.fromList $ map (2020 -) input
        loop [] = Nothing
        loop (x:xs) = case f modInput x of
                        Just sol -> Just sol
                        _ -> loop xs

fromFile :: FilePath -> IO [Int]
fromFile = fmap (map read . lines) . readFile


sums :: [Int] -> IntMap Int
sums input = IM.fromList (sums' input)
 where sums' :: [Int] -> [(Int,Int)]
       sums' [] = []
       sums' (x:xs) = (map (\y -> (x+y,x*y)) xs) ++ sums' xs


solution2 :: [Int] -> Maybe Int
solution2 input = search input
  where im = sums input
        search :: [Int] -> Maybe Int
        search [] = Nothing
        -- Note: check scope of lookup? Report to GHCIDE folks
        search (x:xs) = case IM.lookup (2020 - x) im of
                            Just res -> Just (x*res)
                            _ -> search xs

-- x + y + z = 2020
-- x*y*z = 2020

main :: IO ()
main = do fromFile "input" >>= print . solution
          fromFile "input" >>= print . solution2