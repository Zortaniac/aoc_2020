module Day15 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V
import Utils

type HashTable k v = H.BasicHashTable k v

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
        Nothing -> [0] ++ speak 0 (r+1) mu
        Just ri -> [r-ri] ++ speak (r-ri) (r+1) mu
    where
        mu = Map.alter f x m
        f _ = Just r


speakN :: Int -> Int -> Int -> Map.Map Int Int -> Int
speakN 0 x _ _ = x
speakN i x r m = speakN (i-1) (case (Map.lookup x m) of
        Nothing -> 0
        Just ri -> (r-ri)) (r+1) mu
    where
        mu = Map.alter f x m
        f _ = Just r


speakV :: Int -> Int -> Int -> V.Vector Int -> Int
speakV 0 x _ _ = x
speakV i x r m = speakV (i-1) ru (r+1) mu
    where
        ru = if ri == -1 then 0 else (r-ri)
        ri = (V.!) m x
        mu = (V.//) m [(x, r)]

day15A :: String -> IO ()
day15A i = do
        input <- parseInput parseAll i
        let spoken = speak (last input) (length input -1) $ Map.fromList $ zip (init input) [0..]
        putStrLn $ show $ take 2020 $ spoken

day15B :: String -> IO ()
day15B i = do
        input <- parseInput parseAll i
        --putStrLn $ show $ speakN (30000000 - length input) (last input) (length input -1) $ Map.fromList $ zip (init input) [0..]
        --let spoken = speakV (2020 - length input) (last input) (length input -1) $ (V.//) (V.replicate 2020 0) (zip (init input) [0..])
        let spoken = speakV (30000000 - length input) (last input) (length input -1) $ (V.//) (V.replicate 30000000 (-1)) (zip (init input) [0..])
        putStrLn $ show $ spoken
