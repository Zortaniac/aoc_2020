module Day13 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (sortOn, find)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Utils

parseLineMaybe :: Parser (Maybe Int)
parseLineMaybe = do
        d <- readMaybe <$> (string "x" <|> many1 digit)
        (char ',' <|> newline)
        return d

parseAll :: Parser (Int, [Maybe Int])
parseAll = do
        dep <- read <$> many1 digit
        newline
        lines <- many1 parseLineMaybe
        return (dep, lines)

day13A :: String -> IO ()
day13A i = do
        input <- parseInput parseAll i
        let lines = catMaybes $ snd input
        let dep = fst input
        putStrLn $ show $ t $ head $ sortOn (snd) $ map (f dep) lines
        where
            f d x = (x, x- (d `mod` x))
            t (n, i) = n * i

findRecursive s _ [] = s
findRecursive s r ((x, o):xs) = findRecursive m t xs
        where
            m = head $ filter (\y -> (y+o) `mod` x == 0) [s, (s+r)..]
            t = r * x

day13B :: String -> IO ()
day13B i = do
        input <- catMaybes <$> map f <$> zip [0..] <$> snd <$> parseInput parseAll i
        putStrLn $ show $ findRecursive 1 1 input
        where
            f (_, Nothing) = Nothing
            f (o, (Just x)) = Just (x, o)
