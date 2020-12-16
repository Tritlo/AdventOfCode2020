{-# LANGUAGE  RecordWildCards #-}
module Main where

import Text.Parsec.ByteString
import Text.Parsec
import Text.Parsec.Char
import Debug.Trace
import Data.List
import Data.Function (on)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative ((*>))
type Range = (Int, Int)

parseRange :: Parser Range
parseRange = do from <- many1 digit
                char '-'
                to <- many1 digit
                return (read from, read to)

data Field = Field { key :: String
                   , frst :: Range
                   , scnd ::Range }
   deriving (Show)

type Ticket = [Int]

parseField :: Parser Field
parseField = do key <- manyTill anyChar (char ':')
                spaces
                frst <- parseRange
                space >> string "or" >> space
                scnd <- parseRange
                endOfLine
                return (Field {..})

parseFields :: Parser [Field]
parseFields = manyTill parseField (try endOfLine)


parseTicket :: Parser Ticket
parseTicket = sepBy1 (read <$> many1 digit) (char ',') <* endOfLine

parseYourTicket :: Parser Ticket
parseYourTicket = do string "your ticket:"
                     endOfLine
                     ticket <- parseTicket
                     endOfLine
                     return ticket

parseOtherTickets :: Parser [Ticket]
parseOtherTickets = do string "nearby tickets:" >> endOfLine
                       manyTill parseTicket (try eof)


data Input = Input { fields :: [Field]
                   , yourTicket :: Ticket
                   , otherTickets :: [Ticket]}
  deriving (Show)
parseInput :: Parser Input
parseInput = do fields <- parseFields
                yourTicket <- parseYourTicket
                otherTickets <- parseOtherTickets
                return (Input {..})

solution :: Input -> Int
solution Input{..} =  sum invalid
    where ranges = foldl (\sofar Field{..} -> frst:scnd:sofar) [] fields
          merged_ranges = mergeRanges $ sortBy (compare `on` fst) ranges
          inAnyRange :: Int -> Bool
          inAnyRange i = any (flip contains i) merged_ranges
          invalid :: Ticket
          invalid = filter (not . inAnyRange) $ concat otherTickets

solution2:: Input -> Map String Int
solution2 Input{..} = solve couldBe Map.empty
    where ranges = foldl (\sofar Field{..} -> frst:scnd:sofar) [] fields
          merged_ranges = mergeRanges $  sortBy (compare `on` fst) ranges
          hasInvalid :: Ticket -> Bool
          hasInvalid = not . all inAnyRange
          inAnyRange :: Int -> Bool
          inAnyRange i = any (`contains` i) merged_ranges
          validTickets :: [Ticket]
          validTickets = filter (not . hasInvalid) otherTickets
          t_fields :: [Set Int]
          t_fields =  map Set.fromList $ transpose validTickets
          could_be_field :: Set Int -> Field -> Bool
          could_be_field vals Field{..} = min >= fa && max <= sb
                                        && case min_gt_fb of
                                             Just el -> el >= sa
                                             Nothing -> True
              where ((fa,fb), (sa,sb)) = (frst, scnd)
                    (min, max) = (Set.findMin vals, Set.findMax vals)
                    (_, gt_fb) = Set.split fb vals
                    min_gt_fb = Set.lookupMin gt_fb
          couldBe :: [(Int, Set String)]
          couldBe = zipWith (\i f -> (i,
             Set.fromList $ map key $ filter (could_be_field f) fields)) [1..] t_fields


contains :: Range -> Int -> Bool
contains (a,b) i = a <= i && i <= b

mergeRanges :: [Range] -> [Range]
mergeRanges (a@(ax,bx):b@(ay,by):rest) =
    if (a `contains` ay) || (bx+1) == ay
    then mergeRanges ((ax, max bx by):rest)
    else a:mergeRanges (b:rest)
mergeRanges other = other

solve :: [(Int, Set String)] -> Map String Int -> Map String Int
solve [] sofar = sofar
solve possibilities sofar =
    if Set.size st == 1
    then solve possibilites' $ Map.insert mst i sofar
    else (error $ "set wrong size" ++ show st ++ show ((i,st):rest) ++ show sofar)
  where ((i,st):rest) = sortBy (compare `on` (Set.size . snd)) possibilities
        mst = Set.findMin st
        possibilites' = map (fmap (Set.delete mst)) rest


isDeparture :: String -> Bool
isDeparture = (==) dep . take (length dep)
  where dep = "departure"

main :: IO ()
main = do print "hello, world"
          Right test_input <- parseFromFile parseInput "test-input"
          Right input <- parseFromFile parseInput "input"
          print (solution test_input)
          Right test_input_2 <- parseFromFile parseInput "test-input-2"
          print (solution2 test_input_2)
          let sol = solution2 input
              deps = map snd $ filter (isDeparture . fst) $ Map.toList sol
          let liit :: Int -> Int
              liit i = (yourTicket input) !! (i-1)
          print $ product $ map liit deps

          print (solution2 input)