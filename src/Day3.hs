module Day3 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit

data Tile = Tree | Open  deriving (Show)

parseTile :: Parser Tile
parseTile = do
        c <- char '.' <|> char '#'
        case c of
            '.' -> return Open
            '#' -> return Tree

parseRow :: Parser [Tile]
parseRow = do
         tiles <- many parseTile
         endOfLine
         return tiles

parseAll :: Parser [[Tile]]
parseAll = do
        rows <- many parseRow
        return $ filter ((> 0). length) rows

parseInput :: String -> IO [[Tile]]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

walk :: [[Tile]] -> Int -> (Int, Int) -> Int
walk [] _ _ = 0
walk (x:xs) px (sx, sy) = tree + walk (drop (sy-1) xs) ((px + sx) `mod` length x) (sx, sy)
    where
        isTree Tree = 1
        isTree _ = 0
        tree = isTree (x!!px)


day3A :: String -> IO ()
day3A i = do
        tiles <- parseInput i
        putStrLn $ show $ walk tiles 0 (3, 1)

day3B :: String -> IO ()
day3B i = do
        tiles <- parseInput i
        putStrLn $ show $ product $ map (walk tiles 0) slopes
    where
        slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]