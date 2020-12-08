{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

-- Utils
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond as = case y of
                        [] -> [x]
                        (y:ys) -> x:(splitWhen cond ys)
  where (x,y) = break cond as

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith comp str = take (length comp) str == comp

getInput :: FilePath -> IO [[[String]]]
getInput = fmap (map (splitWhen (== "contain") . words) . lines) . readFile

toBag :: [[String]] -> Map (String, String) (Map (String, String) Int)
toBag [[lightOrDark, color, _], contained] =
    Map.singleton (lightOrDark, color) allContained
  where containedBags = splitWhen (startsWith "bag") contained
        mappify :: [String] -> Map (String, String) Int
        mappify [num, adjective,ccolor] = Map.singleton (adjective, ccolor) (read num)
        mappify _ = Map.empty
        allContained = foldr (Map.union . mappify) Map.empty containedBags

-- {1,2}
ppSet :: Show a => Set a -> String
ppSet set = "{" ++ (reverse r2) ++ "}"
  where ('[':r) = show $ Set.elems set
        (']':r2) = reverse r

transClos :: forall k. forall b. Ord k => Map k (Map k b) -> Map k (Set k)
transClos mp = fixEq update els
  where els :: Map k (Set k)
        els = fmap Map.keysSet mp

update :: forall k . Ord k => Map k (Set k) ->  Map k (Set k)
update curMap = foldr fun curMap (Map.keys curMap)
  where fun :: k -> Map k (Set k) -> Map k (Set k)
        fun k cMap = Map.adjust (Set.union inMap) k cMap
          where els :: [k]
                els = Set.toList $ curMap Map.! k
                inMap :: Set k
                inMap = Set.unions $ mapMaybe (cMap Map.!?) els

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f a = if appl == a then a else fixEq f appl
  where appl = f a

solution :: [[[String]]] -> Int
solution = length .
           filter (("shiny", "gold") `Set.member`) .
           Map.elems . transClos .
           foldr (Map.union . toBag) Map.empty

solution2 :: Map (String, String) (Map (String, String) Int)
          -> (String, String) -> Int
solution2 bags key =
     case bags Map.!? key of
          Nothing -> 0
          Just mp -> sum vals
            where mpA :: [((String, String), Int)]
                  mpA = Map.assocs mp
                  f :: (String, String) -> Int -> Int
                  f _ 0 = 0
                  f k v = v + v*(solution2 bags k)
                  vals :: [Int]
                  vals = map (uncurry f) mpA

type BigMap = Map (String, String) (Map (String, String) Int)
toBigMap :: [[[String]]] -> BigMap
toBigMap = foldr (Map.union . toBag) Map.empty

main :: IO ()
main = do --bigMap <- toBigMap <$> getInput "testInput"
        --   flip mapM_ (Map.assocs bigMap) $ \(k,v) ->
        --       putStrLn ((show k) ++ ":" ++ (show v))
        --   let t_c = transClos bigMap
        --   let canHoldShinyGold =
        --         map fst $ filter (\(k,v)
        --           -> ("shiny", "gold") `Set.member` v)   $ Map.assocs t_c
        --   print canHoldShinyGold
        --   print $ length canHoldShinyGold
          getInput "input" >>= print . solution
        --   print (bigMap Map.!? ("shiny", "gold"))
        --   print (solution2 bigMap ("shiny", "gold"))
        --   bigMapT2 <- toBigMap <$> getInput "testInput2"

        --   print (solution2 bigMapT2 ("shiny", "gold"))
          bigMapInput <- toBigMap <$> getInput "input"
          print (solution2 bigMapInput ("shiny", "gold"))