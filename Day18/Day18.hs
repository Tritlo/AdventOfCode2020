module Main where

import Text.ParserCombinators.ReadP

import Debug.Trace

data Expr = Add Expr Expr
          | Mul Expr Expr
          | Par Expr
          | Digit Int
  deriving (Show)

parseExpr :: ReadP Expr
parseExpr = choice [parsePar,
                    parseOp '+',
                    parseOp '*',
                    parseDigit]
  where parseDigit :: ReadP Expr
        parseDigit = do skipSpaces
                        c <- get
                        return $ Digit $ read [c]
        opToE :: Char -> (Expr -> Expr -> Expr)
        opToE '+' = Add
        opToE '*' = Mul
        digitOrPar = choice [parsePar, parseDigit]
        parsePar :: ReadP Expr
        parsePar = Par <$> (between (char '(') (char ')') (parseExpr))
        parseOp :: Char -> ReadP Expr
        parseOp op = do e1 <- digitOrPar
                        skipSpaces
                        char op
                        skipSpaces
                        e2 <- parseExpr
                        return ((opToE op) e1 e2)

instance Read Expr where
    readsPrec _ = readP_to_S parseExpr


evalExpr :: Expr -> Int
evalExpr (Digit i) = i
evalExpr (Par e) = evalExpr e
evalExpr (Mul a b) = (evalExpr a) * (evalExpr b)
evalExpr (Add a b) = (evalExpr a) + (evalExpr b)

-- 1 + 2 * 3 == Add 1 (Mul 2 3) --> (Mul (Add 1 2) 3)
transPrec :: Expr -> Expr
transPrec (Par e) = Par $ transPrec e
transPrec (Digit i) = Digit i
transPrec (Add a (Mul b c)) = Mul (Add (transPrec a) (transPrec b)) (transPrec c)
transPrec (Add (Mul a b) c) = Mul (Add (transPrec a) (transPrec b)) (transPrec c)
transPrec (Add a b) = if isMul a' || isMul b'
                      then transPrec $ Add a' b'
                      else Add a' b'
  where  a' = transPrec a
         b' = transPrec b
-- 1*2 + 3
transPrec (Mul a b) = Mul (transPrec a) (transPrec b)

isMul :: Expr -> Bool
isMul (Mul _ _) = True
isMul _ = False


getInput :: FilePath -> IO [Expr]
getInput = fmap (map (transPrec . read . traceShowId ) . lines) . readFile
main :: IO ()
main = do getInput "test-input" >>= mapM_ print
          getInput "test-input" >>= print . map evalExpr
          getInput "input" >>= print . sum . map evalExpr