{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Bits
import Data.Array.Unboxed
import Text.Parsec hiding (getInput)
import Text.Parsec.ByteString

import Control.Monad.ST
import Data.Array.ST
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Applicative ((<$))
import Data.Maybe



data Instruction = Mask (Int64 -> Int64)
                 | MemOp Int64 Int64


showMask :: (Show a, Bits t) => (t -> a) -> [Char]
showMask msk = "Mask = " ++ show (msk zeroBits)

instance Show Instruction where
    show (MemOp i n) = "Mem[" ++ show i ++ "] = " ++ show n
    show (Mask msk) = showMask msk


getInput :: FilePath -> IO [Instruction]
getInput fp = do res <- parseFromFile parseIns fp
                 case res of
                    Left err -> error (show err)
                    Right res -> return res

  where parseIns :: Parser [Instruction]
        parseIns = sepBy1 parseIn endOfLine
        parseIn = choice [try parseMask, try parseMemOp]
        parseMask = do string "mask = "
                       maskInfoToMask <$> many1 parseMaskInfo
        parseMaskInfo :: Parser (Maybe Bool)
        parseMaskInfo = choice [ Nothing <$ char 'X'
                               , Just True <$ char '1'
                               , Just False <$ char '0']
        maskInfoToMask :: [Maybe Bool] -> Instruction
        maskInfoToMask msk =
             Mask (foldl (.) id $ map toFun $ mapMaybe dropNothing $ zip [0..] $ reverse msk)
          where dropNothing (_, Nothing) = Nothing
                dropNothing (i, Just x) = Just (i,x)
                toFun (i, b) = flip (if b then setBit else clearBit) i
        parseMemOp :: Parser Instruction
        parseMemOp = do string "mem["
                        addr <- read <$> manyTill digit (try $ char ']')
                        string " = "
                        val <- read <$> many1 digit
                        return (MemOp addr val)


data Memory = Mem { mask :: Int64 -> Int64
                  , mem :: Map Int64 Int64 }
instance Show Memory where
    show (Mem {..}) = showMask mask ++ "\n" ++ show mem

initialMem :: Memory
initialMem = Mem {mask = id, mem = Map.empty}

runInstruction :: Memory -> Instruction -> Memory
runInstruction m (Mask f) = m {mask = f}
runInstruction m@Mem{..} (MemOp addr val) = m {mem = Map.insert addr (mask val) mem}

runProgram :: [Instruction] -> Memory
runProgram = foldl runInstruction initialMem

solution :: [Instruction] -> Int64
solution =  sum . Map.elems .  mem . runProgram


main :: IO ()
main = do
    getInput "test-input" >>= print . solution
    getInput "input" >>= print . solution


-- mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
-- mem[8] = 11
-- mem[7] = 101
-- mem[8] = 0



