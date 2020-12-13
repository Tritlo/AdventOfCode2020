module Main where

import Text.Parsec.ByteString
import Text.Parsec.Char ( digit )
import Text.Parsec

import Data.Char
import Control.Applicative ((<$))
import Data.Maybe
import Debug.Trace
import Data.List
import Data.Function (on)


parseInput :: Parser (Int, [Maybe Int])
parseInput = do id <- parseNumber
                skipMany endOfLine
                timeTable <- sepBy1 (choice [ Just    <$> parseNumber
                                            , Nothing <$ char 'x']) (char ',')
                return (id, timeTable)


parseNumber :: Parser Int
parseNumber = stringToInt <$> many1 digit
stringToInt :: String -> Int
stringToInt = foldl (\x d -> 10*x + digitToInt d) 0

solution1 :: (Int, [Maybe Int]) -> Int
solution1 (sid, timesMaybe) = nextDepartureTime * negate (sid `mod` negate nextDepartureTime)
  where times = catMaybes timesMaybe -- [Int]
        nextDepartureTime = minimumBy (compare `on` (negate . (sid `mod`) . negate)) times

solution2 :: (Int, [Maybe Int]) -> Integer
solution2 (_, timesMaybe) = sum miNis `mod` bigN
  where times = map (\(x, Just y) -> (negate x, toInteger y)) $
                 filter (isJust . snd) $ zip [0..] timesMaybe -- [Int]
        bigN = product $ map snd times
        miNis = map (\(a,ni) -> let bigNi = (bigN `div` ni)
                                in let ~(bigMi,_,1)  = extendedGCD bigNi ni
                                   in a*bigMi*bigNi) times

-- for (a,b) gives (x,y,r) s.t. ax + by = r
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD a b = case extendedGCD' a b 1 0 0 1 of
                    res@(x,y,r) | a*x+b*y == r -> res
                    other -> error ("Invalid GCD!!" ++ show other)
  where extendedGCD' old_r 0 old_s _ old_t _ = (old_s, old_t, old_r)
        extendedGCD' old_r r old_s s old_t t = extendedGCD' r r' s s' t t'
          where q = old_r `div` r
                r' = old_r - q*r
                s' = old_s - q*s
                t' = old_t - q*t




getData :: String -> IO (Int, [Maybe Int])
getData fp = do parse <- parseFromFile parseInput fp
                return $ case parse of
                            Left pe -> error $ show pe
                            Right res -> res

main :: IO ()
main = do getData "test-input" >>= print . solution1
          getData "input" >>= print . solution1
          getData "test-input" >>= print . solution2
          getData "input" >>= print . solution2