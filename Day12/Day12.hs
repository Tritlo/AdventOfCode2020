module Main where

import Data.Complex


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
        (dx :+ dy) = foldl1 (\(a :+ b) (x :+ y) -> (a+x) :+ (b+y)) $
                      map dirToComplex dirs
        (ix :+ iy) = instrsToComplex 0 (0 :+ 0) instr
        degToRot :: Int -> Float
        degToRot 0 = 0
        degToRot 90 = pi/2
        degToRot 180 = pi
        degToRot 270 = 3*pi/2

        instrsToComplex :: Float -> Complex Float -> [Nav] -> Complex Float
        instrsToComplex _ curLoc [] = curLoc
        instrsToComplex curPhase curLoc ((L d):rest)
            = instrsToComplex (curPhase + degToRot d) curLoc rest
        instrsToComplex curPhase curLoc ((R d):rest)
            = instrsToComplex (curPhase - degToRot d) curLoc rest
        instrsToComplex curPhase (cx :+ cy) ((F m):rest)
            = instrsToComplex curPhase ((cx+x) :+ (cy +y)) rest
            where (x :+ y) = mkPolar m curPhase



main :: IO ()
main = do getInput "test-input" >>= print
          getInput "test-input" >>= print . solution
          getInput "input" >>= print . solution