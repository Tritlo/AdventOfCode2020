{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Complex

import Debug.Trace

data Nav = N Float
         | S Float
         | E Float
         | W Float
         | L Int
         | R Int
         | F Float
 deriving (Show)



data Change = Dir (Complex Float)
            | Rot (Complex Float)
            | Move (Complex Float)

 deriving (Show)

degToRot :: Int -> Float
degToRot d = fromIntegral (d `div` 90)*pi/2

navToChange :: Nav -> Change
navToChange (N m) = Dir  $ 0 :+ m
navToChange (S m) = Dir  $ 0 :+ (-m)
navToChange (E m) = Dir  $ m :+ 0
navToChange (W m) = Dir  $ (-m) :+ 0
navToChange (L d) = Rot  $ mkPolar 1.0 (degToRot d)
navToChange (R d) = Rot  $ mkPolar 1.0 (-(degToRot d))
navToChange (F m) = Move $ mkPolar m 0



instance Read Nav where
  readsPrec _ ('N':r) = [( N $ read r, "")]
  readsPrec _ ('S':r) = [( S $ read r, "")]
  readsPrec _ ('E':r) = [( E $ read r, "")]
  readsPrec _ ('W':r) = [( W $ read r, "")]
  readsPrec _ ('L':r) = [( L $ read r, "")]
  readsPrec _ ('R':r) = [( R $ read r, "")]
  readsPrec _ ('F':r) = [( F $ read r, "")]


getInput :: FilePath -> IO [Nav]
getInput = fmap (map read . lines) . readFile

dirToComplex :: Nav -> Complex Float
dirToComplex (N n) = 0 :+ n
dirToComplex (S s) = 0 :+ (-s)
dirToComplex (E e) = e :+ 0
dirToComplex (W w) = (-w) :+ 0
dirToComplex _ = undefined

solution :: [Nav] -> Int
solution navs = round $ abs (dx + ix) + abs (dy + iy)
  where (dirs, instr) = (filter isDir navs, filter (not . isDir) navs)
        (dx :+ dy) = sum $ map dirToComplex dirs
        (ix :+ iy) = instrsToComplex 0 (0 :+ 0) instr

        isDir :: Nav -> Bool
        isDir (N _) = True
        isDir (S _) = True
        isDir (E _) = True
        isDir (W _) = True
        isDir _ = False

        instrsToComplex :: Float -> Complex Float -> [Nav] -> Complex Float
        instrsToComplex _ curLoc [] = curLoc
        instrsToComplex curPhase curLoc ((L d):rest)
            = instrsToComplex (curPhase + degToRot d) curLoc rest
        instrsToComplex curPhase curLoc ((R d):rest)
            = instrsToComplex (curPhase - degToRot d) curLoc rest
        instrsToComplex curPhase ship ((F m):rest)
            = instrsToComplex curPhase (ship+rot) rest
            where rot = mkPolar m curPhase


solution2 :: [Nav] -> Int
solution2 navs = round $ abs ix + abs iy
  where (ix :+ iy) = run 0 (10 :+ 1) $ map navToChange navs
        run :: Complex Float -> Complex Float -> [Change] -> Complex Float
        run ship _ [] = ship
        run ship wp (Dir d:rest) = run ship (wp + d) rest
        run ship wp (Rot r:rest) = run ship (wp*r) rest
        run ship wp (Move m:rest)= run (ship + m*wp) wp rest


main :: IO ()
main = do getInput "test-input" >>= print . solution
          getInput "test-input" >>= print . solution2
          getInput "input" >>= print . solution
          getInput "input" >>= print . solution2