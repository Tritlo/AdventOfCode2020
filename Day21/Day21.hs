module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Data.List
import Data.Function

getInput :: FilePath -> IO [(Set String, Set String)]
getInput = fmap (map toIngrList . lines) . readFile
  where toIngrList :: String -> (Set String, Set String)
        toIngrList str = (Set.fromList ingrs, toAllergs allergs)
          where (ingrs, _:allergs) = break (== "(contains") $ words str
                toAllergs :: [String] -> Set String
                toAllergs = Set.fromList . map init

possiblities :: [(Set String, Set String)] -> Map String (Set String)
possiblities = possiblities' Map.empty
  where possiblities' sofar [] = sofar
        possiblities' sofar ((ingrs, allergs):rest) = possiblities' sofar' rest
            where addToMap ::  Map String (Set String) -> String -> Map String (Set String)
                  addToMap = flip (Map.alter fun)
                    where fun (Just s) = Just $ Set.intersection ingrs s
                          fun _ = Just ingrs
                  sofar' = foldl addToMap sofar allergs

solution :: [(Set String, Set String)] -> Int
solution inp = sum $ map Set.size nonAllergs
    where p = possiblities inp
          allergs = Set.unions $ Map.elems p
          nonAllergs = map ((`Set.difference` allergs) . fst) inp

solve :: Map String (Set String) -> Map String (Set String)
solve = solve' Map.empty
  where solve' sofar pos | Map.null pos = sofar
        solve' sofar pos = solve' (Map.union one sofar) removedOnes
         where (one,more) = Map.partition ((1 ==) . Set.size) pos
               ones = Set.unions $ Map.elems one
               removedOnes = Map.map (`Set.difference` ones) more

solveToOutput  :: Map String (Set String) -> String
solveToOutput mp = intercalate "," assocs
  where assocs = map snd
                  $ sortBy (compare `on` fst)
                  $ map (\(k,v) -> (k, Set.findMin v))
                  $ Map.assocs mp


main :: IO ()
main = do putStrLn "Starting!"
          getInput "test-input" >>= print . solution
          getInput "input" >>= print . solution
          getInput "test-input" >>= print . solveToOutput . solve . possiblities
          getInput "input" >>= print . solveToOutput . solve . possiblities

