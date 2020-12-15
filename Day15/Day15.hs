module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM


import Data.Array.ST
import Control.Monad.ST
import Debug.Trace


getInput :: FilePath -> IO [Int]
getInput = fmap (read . \x -> ('[':x) ++ "]") .readFile


initialMap :: [Int] -> IntMap Int
initialMap input = IM.fromList $ zip input [1..]

update :: (Int, Int, IntMap Int) -> (Int, Int, IntMap Int)
update (spoken, turn, sofar) = --traceShowId $
    case sofar IM.!? spoken of
       Nothing -> (0, turn', sofar' )
       Just lastSpoken -> (turn-lastSpoken, turn', sofar')
    where turn'  = turn + 1
          sofar' = IM.insert spoken turn sofar

solution :: [Int] -> Int
solution input = run (i_spoken, length rest + 1, i_sofar)
  where  (i_spoken:rest) = reverse input
         i_sofar = initialMap $ reverse rest
         run (spoken, 2020, _) = spoken
         run other = run $ update other

input :: [Int]
input = [6,19,0,5,7,13,1]

testInput :: [Int]
testInput = [0,3,6]

sol2Res = 1801753

solution2 :: [Int] -> Int
solution2 input = run (i_spoken, length rest + 1, i_sofar)
  where  (i_spoken:rest) = reverse input
         i_sofar = initialMap $ reverse rest
         run (spoken, 30000000, _) = spoken
         run other = run $ update other


solution2OPTIMIZED :: [Int] -> Int
solution2OPTIMIZED input = runST $
                            initArr (reverse rest)
                            >>= run i_spoken (length rest +1)
  where (i_spoken:rest) = reverse input
        initArr :: [Int] -> ST s (STUArray s Int Int)
        initArr input = do arr <- newArray (0,30000000) 0
                           mapM_ (uncurry (writeArray arr)) (zip input [1..])
                           return arr
        run :: Int -> Int -> STUArray s Int Int -> ST s Int
        run spoken 30000000 _ = return spoken
        run spoken turn arr = do val <- arr `readArray` spoken
                                 writeArray arr spoken turn
                                 run (if val == 0 then 0 else turn-val) (turn+1) arr

main :: IO ()
main = do print (solution input)
          print (solution2OPTIMIZED input)
