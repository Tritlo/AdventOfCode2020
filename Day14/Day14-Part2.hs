{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Bits
import Data.Array.Unboxed
import Text.Parsec hiding (getInput)
import Text.Parsec.ByteString

import Data.Int

import Control.Applicative ((<$))
import Data.Maybe
import Data.Function (on)
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace


data Instruction = Mask  [Maybe Bool]
                 | MemOp Int64 Int64
 deriving (Show)




getInput :: FilePath -> IO [Instruction]
getInput fp = do res <- parseFromFile parseIns fp
                 case res of
                    Left err -> error (show err)
                    Right res -> return res

  where parseIns :: Parser [Instruction]
        parseIns = sepBy1 parseIn endOfLine
        parseIn = choice [try parseMask, try parseMemOp]
        parseMask = do string "mask = "
                       Mask <$> many1 parseMaskInfo
        parseMaskInfo :: Parser (Maybe Bool)
        parseMaskInfo = choice [ Nothing <$ char 'X'
                               , Just True <$ char '1'
                               , Just False <$ char '0']
        -- maskInfoToMask :: [Maybe Bool] -> Instruction
        -- maskInfoToMask msk =
        --      Mask (foldl (.) id $ map toFun $ mapMaybe dropNothing $ zip [0..] $ reverse msk)
        --   where dropNothing (_, Nothing) = Nothing
        --         dropNothing (i, Just x) = Just (i,x)
        --         toFun (i, b) = flip (if b then setBit else clearBit) i
        parseMemOp :: Parser Instruction
        parseMemOp = do string "mem["
                        addr <- read <$> manyTill digit (try $ char ']')
                        string " = "
                        val <- read <$> many1 digit
                        return (MemOp addr val)


data Block = Block { mask :: [Maybe Bool]
                   , memOps :: [(Int64, Int64)]}
  deriving (Show)

toBlocks :: [Instruction] -> [Block]
toBlocks [] = []
toBlocks ((Mask msk):ins) = Block msk (map toPair first):toBlocks rest
  where (first, rest) = break isMask ins
        isMask (Mask _) = True
        isMask _ = False
        toPair (MemOp a b) = (a,b)
        toPair _ = error "Not applied to MemOP!"


blockSum :: Block -> (Set Int64, Int64) -> (Set Int64, Int64)
blockSum Block{..} init = foldl' addAndUpdate init deduped
  where addrSet = maskToAddresses mask
        addAndUpdate (covered, cur_sum) (addr, val) = (covered', cur_sum+totalValAdded)
          where addrs = addrSet addr `Set.difference` covered
                totalValAdded = fromIntegral (length addrs)*val
                covered' = covered `Set.union` addrs
        deduped = nubBy ((==) `on` fst) $ reverse memOps
        maskToAddresses :: [Maybe Bool] -> (Int64 -> Set Int64)
        maskToAddresses msk = func
            where f (_, Just False) = Just id
                  f (i, Just True)  = Just $ flip setBit i
                  f _ = Nothing
                  maskInds = zip [0..] (reverse msk)
                  base :: Int64 -> Int64
                  base = foldl (.) id $ mapMaybe f maskInds
                  xLocs = map fst $ filter (isNothing . snd) maskInds
                  updXLoc :: [Int] -> Set Int64 -> Set Int64
                  updXLoc [] sofar = sofar
                  updXLoc (i:is) sofar = updXLoc is sofar'
                    where sofar' = Set.map (`setBit` i) sofar
                                    `Set.union`
                                    Set.map (`clearBit` i) sofar
                  func = updXLoc xLocs  . Set.singleton  . base


main :: IO ()
main = do --getInput "test-input-2" >>= print . snd .  foldl' (flip blockSum) (Set.empty, 0) . reverse . toBlocks
          getInput "input" >>= print . snd . foldl' (flip blockSum) (Set.empty, 0) . reverse . toBlocks




