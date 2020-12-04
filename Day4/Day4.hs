{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Char (isDigit)

import Control.Monad
import Text.Read


type Passport = Map String String

toPassport :: [String] -> Passport
toPassport = undefined

getInput :: FilePath -> IO [Passport]
getInput fp = do lns <- lines <$> readFile fp
                 let pEntries = splitWhen (== "") lns
                     compPs = map (map (break (== ':')) . concat . (map words)) pEntries
                     ps = map (Map.map (drop 1) . Map.fromList) compPs
                 return ps

splitWhen :: (a -> Bool) -> [a] ->[[a]]
splitWhen cond xs = case break cond xs of
                       (first, _:rest) -> first:(splitWhen cond rest)
                       (first, _) -> [first]

isValid1 :: Passport -> Bool
isValid1 ps = case length ps of
                8 -> True
                7 -> not ("cid" `Map.member` ps)
                _ -> False


data ValidPassport = VP { byr :: String
                        , iyr :: String
                        , eyr :: String
                        , hgt :: String
                        , hcl :: String
                        , ecl :: String
                        , pid :: String }
  deriving (Show)

toValidPassport :: Map String String -> ValidPassport
toValidPassport m = VP { byr = m Map.! "byr"
                       , iyr = m Map.! "iyr"
                       , eyr = m Map.! "eyr"
                       , hgt = m Map.! "hgt"
                       , hcl = m Map.! "hcl"
                       , ecl = m Map.! "ecl"
                       , pid = m Map.! "pid" }


validHclChars :: Set Char
validHclChars = Set.fromList (['0'..'9'] ++ ['a'..'f'])

validEclVals :: Set String
validEclVals = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

hasValidFields :: ValidPassport -> Bool
hasValidFields (VP {..}) = and [ validByr, validIyr, validEyr
                               , validHgt, validHcl, validEcl
                               , validPid]
    where checkYear :: Int -> Int -> String -> Bool
          checkYear l h str = length str == 4 &&
                     case readMaybe @Int str of
                        Just i -> i >= l && i <= h
                        _ -> False
-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
          validByr = checkYear 1920 2002 byr
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
          validIyr = checkYear 2010 2020 iyr
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
          validEyr = checkYear 2020 2030 eyr
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
          validHgt = case reverse hgt of
                        'm':'c':r -> check 150 193 r
                        'n':'i':r -> check 59 76 r
                        _ -> False
            where check lo hi r = case readMaybe @Int (reverse r) of
                                    Just h -> h >= lo && h <= hi
                                    _ -> False
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f
          validHcl = length hcl == 7 &&
                     case hcl of
                         '#':r -> all (`Set.member` validHclChars) r
                         _ -> False
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
          validEcl = ecl `Set.member` validEclVals
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
          validPid = length pid == 9 && all isDigit pid


allValid :: FilePath -> IO [Bool]
allValid inp = do testPassports <- getInput inp
                  print (length $ filter id $ map isValid1 $ testPassports)
                  let validTestPassports = map toValidPassport $ filter isValid1 testPassports
                  return $ map hasValidFields validTestPassports

main :: IO ()
main = do testPassports <- getInput "testInput"
          print (length $ filter id $ map isValid1 $ testPassports)
          let validTestPassports = map toValidPassport $ filter isValid1 testPassports
          print (length $ filter hasValidFields validTestPassports)

          allValid "testInput" >>= print
          allValid "invalidPass" >>= print
          allValid "validPass" >>= print

          passports <- getInput "input"
          let validPassports = map toValidPassport $ filter isValid1 passports
          print (length $ validPassports)
          print (length $ filter hasValidFields validPassports)
