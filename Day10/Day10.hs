{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

getInput :: FilePath -> IO [Int]
getInput = fmap (map (read @Int) . lines) . readFile


sol1 :: [Int] -> Int
sol1 inp = (mp' Map.! 1) * (mp' Map.! 3)
  where s = sort (0:inp)
        f li@(x:_) = (x, length li)
        mp = Map.fromList $ map f $ group $ sort $ zipWith (-) (tail s) s
        mp' = Map.adjust (+1) 3 mp

sol2 :: [Int] -> Int
sol2 input = snd (sol2' Map.empty 0)
  where inputSet = Set.fromList input'
        end = (maximum input) + 3
        input' = (0:end:input)
        sol2' :: Map Int Int -> Int -> (Map Int Int, Int)
        sol2' memo cur =
           if cur `Map.member` memo then (memo, memo Map.! cur)
           else case (filter (`Set.member` inputSet) [cur+1..cur+3]) of
                    [] -> (Map.insert cur 1 memo, 1)
                    outs -> let (m', res) = foldl f (memo, 0) outs
                            in (Map.insert cur res m', res)
              where f :: (Map Int Int, Int) -> Int -> (Map Int Int, Int)
                    f (memo, cur_sum) out = (m', cur_sum + out_val)
                        where (m', out_val) = sol2' memo out




main :: IO ()
main = do getInput "test-small" >>= print  . sol1
          getInput "test-big" >>= print  . sol1
          getInput "input" >>= print  . sol1
          getInput "test-small" >>= print  . sol2
          getInput "test-big" >>= print  . sol2
          getInput "input" >>= print  . sol1
          (sol2 <$> getInput "input") >>= print