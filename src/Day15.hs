module Day15 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Utils

parseNum :: Parser Int
parseNum = do
        n <- read <$> many1 digit
        char ',' <|> newline
        return n


parseAll :: Parser [Int]
parseAll = do
        xs <- many1 parseNum
        return xs

speak :: Int -> Int -> Map.Map Int Int -> [Int]
speak x r m = case (Map.lookup x m) of
        Nothing -> [0] ++ speak 0 (r+1) (Map.insert x r m)
        Just ri -> [r-ri] ++ speak (r-ri) (r+1) (Map.update f x m)
    where
        f _ = Just r

day15A :: String -> IO ()
day15A i = do
        input <- parseInput parseAll i
        let spoken = speak (last input) (length input -1) $ Map.fromList $ zip (init input) [0..]
        putStrLn $ show $ head $ drop 2019 $ input ++ spoken

day15B :: String -> IO ()
day15B i = do
        input <- parseInput parseAll i
        let spoken = speak (last input) (length input -1) $ Map.fromList $ zip (init input) [0..]
        putStrLn $ show $ head $ drop 29999999 $ input ++ spoken
