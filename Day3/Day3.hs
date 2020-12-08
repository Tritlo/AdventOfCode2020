module Main where


getInput :: FilePath -> IO [String]
getInput = fmap lines . readFile

-- allCycled :: [String] -> [String]
-- allCycled = map cycle

-- i = 0, 3, 6, 9,

-- takeEveryThird [0..10] = [3,6,9]



takeEveryN :: Int -> [a] -> [a]
takeEveryN n xs = case drop (n-1) xs of
                    (y:ys) -> y:(takeEveryN n ys)
                    _ -> []


-- Right 3 Down 1
coords :: [Int]
coords = 0:(takeEveryN 3 [1..])

lookupInInput :: [String] -> [Int] -> [Char]
lookupInInput [] _ = []
lookupInInput (ln:lns) (c:cs) = (ln !! (c `mod` (length ln))):(lookupInInput lns cs)

isTree :: Char -> Bool
isTree '#' = True
isTree _ = False

countTrees :: [String] -> Int
countTrees input = length $ filter isTree $ lookupInInput input coords

coordsGen :: Int -> [Int]
coordsGen right = 0:(takeEveryN right [1..])

countTreesGen :: [String] -> (Int, Int) -> Int
countTreesGen (i:is) (right,down) = length $ filter isTree $ lookupInInput (i:(takeEveryN down is)) (coordsGen right)

cases :: [(Int, Int)]
cases = [ (1,1)
        , (3,1)
        , (5,1)
        , (7,1)
        , (1,2) ]

main :: IO ()
main = do getInput "testInput" >>= print . countTrees
          getInput "input" >>= print . countTrees
          ti <- getInput "testInput"
          print $ product (map (countTreesGen ti) cases)
          inp <- getInput "input"
          print $ product (map (countTreesGen inp) cases)