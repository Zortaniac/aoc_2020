module Day2 where
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import System.Exit

data Policy = Policy Int Int Char deriving (Show)

data Password = Password Policy String deriving (Show)

parsePolicy :: Parser Policy
parsePolicy = do
        min <- many digit
        char '-'
        max <- many digit
        space
        c <- letter
        return $ Policy (read min) (read max) c

parsePassword :: Parser Password
parsePassword = do
         p <- parsePolicy
         char ':'
         space
         pass <- many (letter <|> digit)
         return $ Password p pass

readPassword input = case parse parsePassword "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

parseInput :: String -> IO [Password]
parseInput s = mapM readPassword $ lines s

checkPolicy :: Policy -> String -> Bool
checkPolicy (Policy min max c) s = min <= count && count <= max
    where
        count = cl s c
        cl str c = length $ filter (== c) str

isValid :: Password -> Bool
isValid (Password p s) = checkPolicy p s

day2A :: String -> IO ()
day2A i = do
        passwords <- parseInput i
        putStrLn $ show $ length $ filter isValid passwords



checkPolicy' :: Policy -> String -> Bool
checkPolicy' (Policy posA posB c) s = (a || b) && not (a == b)
    where
        a = (s !! (posA -1)) == c
        b = (s !! (posB -1)) == c

isValid' :: Password -> Bool
isValid' (Password p s) = checkPolicy' p s

day2B :: String -> IO ()
day2B i = do
        passwords <- parseInput i
        putStrLn $ show $ length $ filter isValid' passwords
