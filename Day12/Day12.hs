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

isDir :: Nav -> Bool
isDir (N _) = True
isDir (S _) = True
isDir (E _) = True
isDir (W _) = True
isDir _ = False

isF :: Nav -> Bool
isF (F _) = True
isF _ = False


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
        degToRot :: Int -> Float
        degToRot d = fromIntegral (d `div` 90)*pi/2

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
  where (ix :+ iy) = run 0 (10 :+ 1) navs
        degToRot :: Int -> Float
        degToRot d = fromIntegral (d `div` 90)*pi/2

        dirToMove :: Nav -> Complex Float
        dirToMove (N m) = 0 :+ m
        dirToMove (S m) = 0 :+ (-m)
        dirToMove (E m) = m :+ 0
        dirToMove (W m) = (-m) :+ 0

        rotToChange :: Nav -> Complex Float
        rotToChange (L d) = mkPolar 1.0 (degToRot d)
        rotToChange (R d) = mkPolar 1.0 (-(degToRot d))

        fToMove :: Nav -> Complex Float
        fToMove (F m) = mkPolar m 0

        run :: Complex Float -> Complex Float -> [Nav] -> Complex Float
        run ship _ [] = ship
        run ship wp (ins:rest) | isDir ins = run ship (wp + dirToMove ins) rest
        run ship wp (ins:rest) | isF ins = run (ship + fToMove ins*wp) wp rest
        run ship wp (ins:rest) = run ship (wp*rotToChange ins) rest


main :: IO ()
main = do getInput "test-input" >>= print . solution
          getInput "test-input" >>= print . solution2
          getInput "input" >>= print . solution
          getInput "input" >>= print . solution2