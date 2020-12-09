module Day9 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit

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


perm (x:xs) = [x+y | y <- xs] ++ (perm xs)
perm [] = []

findFirstIssue :: [Int] -> Int -> Int
findFirstIssue xs n =
    if match then
        findFirstIssue (tail xs) n
    else
        x
    where
        x = xs!!n
        firstN = take n xs
        match = x `elem` (perm firstN)

findContiguousSet :: [Int] -> [Int] -> [Int] -> Int -> Int
findContiguousSet (_:os) [] ts n = findContiguousSet os os [] n
findContiguousSet os (x:xs) ts n =
    if match then
        result
    else
        findContiguousSet os xs t n
    where
        result = (foldr1 min t) + (foldr1 max t)
        match = (length t > 1) && n == (foldr (+) x ts)
        t = x:ts

day9A :: String -> IO ()
day9A i = do
        input <- parseInput i
        putStrLn $ show $ findFirstIssue input 25

day9B :: String -> IO ()
day9B i = do
        input <- parseInput i
        putStrLn $ show $ findContiguousSet input input [] $ findFirstIssue input 25


