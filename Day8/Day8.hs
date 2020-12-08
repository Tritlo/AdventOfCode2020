{-# LANGUAGE TypeApplications, RecordWildCards #-}
module Main where

import Text.ParserCombinators.ReadP
import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array
import Data.Maybe
import Data.Tuple

getInput :: FilePath -> IO Program
getInput fp = do str <- readFile fp
                 let instrs = map (read @Instruction) $ lines str
                 return $ listArray (0, length instrs - 1) instrs

data Instruction = NOP Int
                 | ACC Int
                 | JMP Int
  deriving (Show)

parseInstruction :: ReadP Instruction
parseInstruction = do code <- parseCode
                      skipSpaces
                      val <- parseInstrInt
                      return (code val)
   where parseCode :: ReadP (Int -> Instruction)
         parseCode = choice [ fromString "nop" NOP
                            , fromString "acc" ACC
                            , fromString "jmp" JMP ]
         fromString :: String -> (Int -> Instruction)
                     -> ReadP (Int -> Instruction)
         fromString str ins = string str >> pure ins
         parseInstrInt :: ReadP Int
         parseInstrInt = optional (char '+') >> readS_to_P (reads @Int)


instance Read Instruction where
  readsPrec _ = readP_to_S parseInstruction

data VirtualMachine = VM { accumulator :: Int
                         , instr :: Int }

initialVMState :: VirtualMachine
initialVMState = VM {accumulator = 0, instr = 0}

runInstr :: Instruction -> VirtualMachine -> VirtualMachine
runInstr (NOP _) vm@(VM {..}) = vm {instr = instr + 1}
runInstr (ACC a) vm@(VM {..}) = vm { instr = instr + 1 , accumulator = accumulator + a}
runInstr (JMP j) vm@(VM {..}) = vm {instr = instr + j}

type Program = Array Int Instruction

runProgram :: Program -> Int
runProgram progArr = run initialVMState Set.empty
    where run :: VirtualMachine -> Set Int -> Int
          run vm@(VM {..}) visited =
              if instr `Set.member` visited
              || let (l,h) = bounds progArr in instr < l && instr >= h
              then accumulator
              else let curI = progArr ! instr
                       vm' = runInstr curI vm
                       visited' = instr `Set.insert` visited
                   in run vm' visited'

runProgramMaybe :: Program -> VirtualMachine -> Maybe Int
runProgramMaybe progArr ivm = run ivm Set.empty
    where run :: VirtualMachine -> Set Int -> Maybe Int
          run vm@(VM {..}) visited =
              if instr `Set.member` visited
              then Nothing
              else if instr == (snd $ bounds $ progArr)
                   then Just accumulator
                   else let curI = progArr ! instr
                            vm' = runInstr curI vm
                            visited' = instr `Set.insert` visited
                         in run vm' visited'

runAndFix :: Program -> Maybe Int
runAndFix progArr = run initialVMState Set.empty
    where run :: VirtualMachine -> Set Int -> Maybe Int
          run vm@(VM {..}) visited =
              if instr `Set.member` visited
              then Nothing
              else if instr == (snd $ bounds $ progArr)
                   then Just accumulator
                   else let curI = progArr ! instr
                            vm' = runInstr curI vm
                            visited' = instr `Set.insert` visited
                            continue = run vm' visited'
                         in case curI of
                               ACC _ -> continue
                               JMP n -> let pA' = progArr // [(instr, NOP n)]
                                        in case runProgramMaybe pA' vm of
                                          Just acc -> Just acc
                                          Nothing -> continue
                               NOP n -> let pA' = progArr // [(instr, JMP n)]
                                        in case runProgramMaybe pA' vm of
                                          Just acc -> Just acc
                                          Nothing -> continue

reachable :: Program -> Set Int
reachable progArr = run initialVMState Set.empty
    where run :: VirtualMachine -> Set Int -> Set Int
          run vm@(VM {..}) visited =
              if instr `Set.member` visited then visited
              else let curI = progArr ! instr
                       vm' = runInstr curI vm
                       visited' = instr `Set.insert` visited
                   in run vm' visited'

ppSet :: Show a => Set a -> String
ppSet set = "{" ++ (reverse r2) ++ "}"
  where ('[':r) = show $ Set.elems set
        (']':r2) = reverse r


data PreComputed =
     PC { n' :: Map Int Int
        , a' :: Map Int Int
        , j  :: Map Int Int
        , j'  :: Map Int Int
        , r  :: Set Int }
  deriving (Show)

modifyArr :: Ix i => Array i e -> i -> (e -> e) -> Array i e
modifyArr arr loc f = arr // [(loc, f (arr ! loc))]

flipProg :: Program -> Int -> Program
flipProg prog loc = modifyArr prog loc flip
    where flip (JMP n) = NOP n
          flip (NOP n) = JMP n
          flip _ = error "trying wrong flip!"

preCompute :: Program -> PreComputed
preCompute prog = PC {..}
  where r = reachable prog
        n' = Map.fromList $ map swap $ filter ((`Set.member` r) . fst) nops
        a' = Map.fromList $ map swap (accs ++ nops)
        j  = Map.fromList $ filter ((`Set.member` r) . fst) jmps
        j' = Map.fromList $ map swap jmps

        (jmps, nops, accs) = go [] [] [] $ assocs prog
        go js ns as [] = (js, ns, as)
        go js ns as (((i, instr)):rest) =
             case instr of
                 JMP n -> go ((i,i+n):js) ns as rest
                 NOP n -> go js ((i,i+n):ns) as rest
                 ACC _ -> go js ns ((i,i+1):as) rest


reverseEngineer :: PreComputed -> Program -> Int -> Maybe Int
reverseEngineer pc@PC{..} prog eop =
    case a' Map.!? eop of
        Just i -> reverseEngineer pc prog i
        _ -> case (n' Map.!? eop) of
                Just i -> Just i
                Nothing ->
                    case (j Map.!? (eop-1)) of
                         Just i -> Just (eop-1)
                         _ -> case (j' Map.!? eop) of
                                Just i -> reverseEngineer pc prog i
                                Nothing -> Nothing

main :: IO ()
main = do getInput "testInput" >>= print . runProgram
          getInput "input" >>= print . runProgram
          --Dynamic Program Analysis
          getInput "testInput" >>= print . runAndFix
          getInput "input" >>= print . runAndFix

          -- Static Program Analysis
          prog <- getInput "testInput"
          let pc = preCompute prog
              (_, u) = bounds prog
          print (reverseEngineer pc prog (u+1))

          prog2 <- getInput "input"
          let pc2 = preCompute prog2
              (_, u2) = bounds prog2
              Just iToR = reverseEngineer pc2 prog2 (u2+1)
              nprog = flipProg prog2 iToR
          print $ runProgramMaybe nprog initialVMState