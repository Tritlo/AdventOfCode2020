module Main where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import Data.Foldable (toList)
import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import qualified Data.Map as Map
import Data.Map (Map)

getInput :: FilePath  -> IO ([Int], [Int])
getInput = fmap toPir . readFile
  where toPir :: String -> ([Int],[Int])
        toPir str = (map read p1, map read p2)
            where (_:p1, _:_:p2) = break (== "") $ lines str

score :: [Int] -> Int
score final = sum $ zipWith (\i n -> (lf-i)*n) [0..] final
  where lf = length  final

resultShouldBe :: [Int]
resultShouldBe = [3, 2, 10, 6, 8, 5, 9, 4, 7, 1]

simulate :: (Seq Int, Seq Int) -> [Int]
simulate (Seq.Empty, xs) = toList xs
simulate (xs, Seq.Empty) = toList xs
simulate (a:<|as, b:<|bs) = if a > b
                            then simulate (as:|>a:|>b, bs)
                            else simulate (as, bs:|>b:|>a)

toSeqs :: ([a], [a]) -> (Seq a, Seq a)
toSeqs (a, b) = (Seq.fromList a, Seq.fromList b)

type Deck = Seq Int
type Decks = (Deck, Deck)



recursiveCombat :: Set Decks -> Map Decks Int -> Decks -> (Int, Map Decks Int, Decks)
recursiveCombat _ memo d | d `Map.member` memo = (memo Map.! d, memo, d)
recursiveCombat _ memo ds@(d1, Seq.Empty) = (0, memo, ds)
recursiveCombat _ memo ds@(Seq.Empty, d2) = (1, memo, ds)
recursiveCombat prev memo d@(d1:<|d1s,d2:<|d2s)
    | d `Set.member` prev =  (0, Map.insert d 0 memo, d)
recursiveCombat prev memo d@(d1:<|d1s, d2:<|d2s)
    | d1 <= Seq.length d1s && d2 <= Seq.length d2s =
        case recursiveCombat prev memo (d1s, d2s) of
            (0, m', _) -> recursiveCombat (d `Set.insert` prev) (Map.insert sub 0 m') (d1s:|>d1:|>d2, d2s)
            (1, m', _) -> recursiveCombat (d `Set.insert` prev) (Map.insert sub 1 m') (d1s, d2s:|>d2:|>d1)
    where sub = (d1s, d2s)
recursiveCombat prev memo d@(d1:<|d1s, d2:<|d2s) =
    recursiveCombat (d `Set.insert` prev) memo $
            if d1 > d2 then  (d1s:|>d1:|>d2, d2s)
                       else  (d1s, d2s:|>d2:|>d1)

getWinner :: (Int, Map Decks Int, Decks) -> [Int]
getWinner (0, _, (as,_)) = toList as
getWinner (1, _, (_, bs)) = toList bs

main :: IO ()
main = do getInput "test-input" >>= print . score . simulate . toSeqs
          getInput "test-input" >>= print . score. getWinner . recursiveCombat Set.empty Map.empty . toSeqs
          getInput "input" >>= print . score . simulate . toSeqs
          getInput "input" >>= print . score. getWinner . recursiveCombat Set.empty Map.empty . toSeqs