module Day6 where
import Data.Char
import Data.List ( nub, intersect )
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Data.Sort

data Answer = Answer [Char] deriving (Show)
data Group = Group [Answer]  deriving (Show)

split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

parseAnswer :: Parser Answer
parseAnswer = do
        s <- many1 letter
        newline
        return $ Answer s

parseGroup ::Parser Group
parseGroup = do
        answers <- many1 parseAnswer
        newline
        return $ Group answers

parseAll :: Parser [Group]
parseAll = do
        groups <- many1 parseGroup
        return groups

parseInput :: String -> IO [Group]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

unbox (Answer a) = a

uniqueAnswerCount :: Group -> Int
uniqueAnswerCount (Group answers) = length $ nub $ concat $ map unbox answers

sameAnswerCount :: Group -> Int
sameAnswerCount (Group answers) = length $ foldr intersect (head uba) (tail uba)
                            where
                                uba = map unbox answers

day6A :: String -> IO ()
day6A i = do
        groups <- parseInput i
        putStrLn $ show $ sum $ map uniqueAnswerCount groups

day6B :: String -> IO ()
day6B i = do
        groups <- parseInput i
        putStrLn $ show $ sum $ map sameAnswerCount groups

