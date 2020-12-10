module Day10 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Data.List (sort, subsequences, group)

parseRow :: Parser Int
parseRow = do
        num <- many1 digit
        newline
        return $ read num

parseAll :: Parser [Int]
parseAll = do
        row <- many1 parseRow
        return row

parseInput :: String -> IO [Int]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

getJoltRating = (+ 3) . maximum

diff1 1 = 1
diff1 _ = 0

diff3 3 = 1
diff3 _ = 0

count1JoltDiff (x:y:[]) = diff1 (y-x)
count1JoltDiff (x:y:xs) = (diff1 (y-x)) + count1JoltDiff (y:xs)

count3JoltDiff (x:y:[]) = diff3 (y-x)
count3JoltDiff (x:y:xs) = (diff3 (y-x)) + count3JoltDiff (y:xs)

day10A :: String -> IO ()
day10A i = do
        input <- sort <$> parseInput i
        let i = (0:input) ++ [getJoltRating input]
        putStrLn $ show $ (count1JoltDiff i) * (count3JoltDiff i)

day10B :: String -> IO ()
day10B i = do
        input <- sort <$> parseInput i
        putStrLn $ show $ product $ map y $ filter (\x -> (head x) == 1 ) $ group $ map (\x -> snd x - fst x ) $ zip (0:input) input
    where
        y x = case length x of
            1 -> 1
            2 -> 2
            3 -> 4
            4 -> 7

