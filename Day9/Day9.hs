{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

-- Utils
mapAntitonic :: Eq b => (a -> b) -> Set a -> Set b
mapAntitonic f = Set.fromDescList . map f . Set.toAscList

ppSet :: Set Int -> String
ppSet set = "{" ++ (reverse r2) ++ "}"
  where ('[':r) = show $ Set.elems set
        (']':r2) = reverse r

getInput :: FilePath -> IO [Int]
getInput = fmap (map (read @Int) . lines) . readFile

solution :: Int -> [Int] -> Int
solution pre_len input = solution' input preamble rest
    where (pre, rest) = splitAt pre_len input
          preamble = Set.fromList pre
          solution' :: [Int] -> Set Int -> [Int] -> Int
          solution' (to_drop:to_drops) preamble (to_check:to_checks)
            = if preamble `Set.disjoint` check_set
              then to_check else solution' to_drops preamble' to_checks
            where check_set = mapAntitonic (to_check -) preamble
                  preamble' = to_check `Set.insert` (to_drop `Set.delete` preamble)
          solution' _ _ _ = error "Nothing to check and/or drop!"

solution2 :: Int -> [Int] -> Seq Int
solution2 looking_for (a:b:res) = solution2' looking_for (a+b) (Seq.fromList [a,b]) res
  where solution2' :: Int -> Int -> Seq Int -> [Int] -> Seq Int
        solution2' looking_for cur_sum els@(c Seq.:<| cs) inc@(i:is) =
          if cur_sum == looking_for then els
          else if cur_sum > looking_for
              then solution2' looking_for (cur_sum - c) cs inc
              else solution2' looking_for (cur_sum + i) (els :|> i) is

--solution2 looking_for -> [Int]

main :: IO ()
main = do testInput <- getInput "testInput"
          input <- getInput "input"
          let sol1 = solution 5 testInput
              sol2 = solution 25 input
          print sol1
          print sol2
          let contSet = solution2 sol2 input
              res = minimum contSet  + maximum contSet
          print $ sum contSet
          print res


