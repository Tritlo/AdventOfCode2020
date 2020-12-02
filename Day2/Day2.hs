module Main where

import Data.Char

-- n-n l: password

fromFile :: FilePath -> IO [String]
fromFile = fmap lines . readFile

parse :: String -> (((Int, Int), Char), String)
parse s = (((read mi, read ma), c), pass)
  where [pol, c:':':[], pass] = words s
        (mi,'-':ma) = span (/= '-') pol

check :: (Int, Int) -> Char -> String -> Int -> Bool
check (mi, ma) _  [] sofar = mi <= sofar
check p@(mi, ma) c (x:xs) sofar | x == c && sofar < ma = check p c xs (sofar+1)
                                | x == c = False
                                | otherwise = check p c xs sofar

checkPass :: String -> Bool
checkPass = flip (uncurry $ uncurry check) 0 . parse

xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True
xor True True = False

checkPass2 :: String -> Bool
checkPass2 s = ((pass !! (p1-1)) == c) `xor` ((pass !! (p2-1)) == c)
  where (((p1,p2), c), pass) = parse s


main :: IO ()
main = do fromFile "testInput" >>= print . length . filter id . map checkPass
          fromFile "input" >>= print . length . filter id . map checkPass
          fromFile "testInput" >>= print . length . filter id . map checkPass2
          fromFile "input" >>= print . length . filter id . map checkPass2