module Day22 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (find)
import Utils

parseInt :: Parser Int
parseInt = do
        d <- read <$> many1 digit
        newline
        return d

parseAll :: Parser ([Int], [Int])
parseAll = do
        string "Player 1:"
        newline
        p1 <- many1 parseInt
        newline
        string "Player 2:"
        newline
        p2 <- many1 parseInt
        return (p1, p2)

play :: [Int] -> [Int] -> [Int]
play [] xs = xs
play xs [] = xs
play (x:xs) (y:ys) = if x > y
                     then play (xs ++ [x, y]) ys
                     else play xs (ys ++ [y, x])

day22A :: String -> IO ()
day22A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sum $ map f $ zip [1..] $ reverse $ play (fst input) (snd input)
    where
        f (x, y) = x * y

playRecursive :: [Int] -> [Int] -> [([Int], [Int])] -> ([Int], [Int])
playRecursive [] xs _ = ([], xs)
playRecursive xs [] _ = (xs, [])
playRecursive (x:xs) (y:ys) rs = case find f rs of
                                    Just _ -> ((x:xs), [])
                                    Nothing -> if enoughCards then w2 else w1
         where
            r = ((x:xs), (y:ys))
            rst = (r:rs)
            enoughCards = length xs >= x && length ys >= y
            w1 = if x > y then playRecursive (xs ++ [x, y]) ys rst else playRecursive xs (ys ++ [y, x]) rst
            w2 = case playRecursive (take x xs) (take y ys) [] of
                    ([], _) -> playRecursive xs (ys ++ [y, x]) rst
                    (_, []) -> playRecursive (xs ++ [x, y]) ys rst
            f x = x == r


day22B :: String -> IO ()
day22B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sum $ map f $ zip [1..] $ reverse $ q $ playRecursive (fst input) (snd input) []
    where
        f (x, y) = x * y
        q (x, y) = x ++ y
