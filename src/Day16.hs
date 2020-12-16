module Day16 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (isPrefixOf, find)
import Utils

data Field = Field String [Int] deriving Show

data Input = Input [Field] [Int] [[Int]] deriving Show

parseNum :: Parser Int
parseNum = read <$> many1 digit

parseNumList :: Parser [Int]
parseNumList = do
        l <- sepBy parseNum (char ',')
        newline
        return l

parseRange :: Parser [Int]
parseRange = do
        s <- read <$> many1 digit
        char '-'
        e <- read <$> many1 digit
        return [s..e]

parseField :: Parser Field
parseField = do
        name <- many1 (letter <|> char ' ')
        string ": "
        r1 <- parseRange
        string " or "
        r2 <- parseRange
        newline
        return $ Field name $ r1 ++ r2


parseAll :: Parser Input
parseAll = do
        fs <- many1 parseField
        newline
        string "your ticket:"
        newline
        l <- parseNumList
        newline
        string "nearby tickets:"
        newline
        ls <- many1 parseNumList
        return $ Input fs l ls

getData :: Input -> [[Int]]
getData (Input _ _ ls) = ls

getSeatData :: Input -> [Int]
getSeatData (Input _ xs _) = xs


getFields :: Input -> [Field]
getFields (Input fs _ _) = fs

getFieldName :: Field -> String
getFieldName (Field s _) = s

getErrorRate :: [Field] -> [Int] -> Int
getErrorRate _ [] = 0
getErrorRate fs (x:xs) = f fs + getErrorRate fs xs
                    where
                        f [] = x
                        f ((Field _ rs):ys) = if x `elem` rs then 0 else f ys

combine :: [[Int]] -> [[Int]]
combine ([]:xs) = []
combine xs = [map head xs] ++ (combine $ map tail xs)

identifyFieldName :: [Field] -> [Int] -> [String]
identifyFieldName [] _ = []
identifyFieldName ((Field s ys):fs) xs = (if match then [s] else []) ++ (identifyFieldName fs xs)
                     where
                         match = all (== True) $ map (`elem` ys) xs

identifyFieldNames :: [Field] -> [[Int]] -> [[String]]
identifyFieldNames _ [] = []
identifyFieldNames [] _ = []
identifyFieldNames fs (x:xs) = (name:(identifyFieldNames fs xs))
                    where
                        name = identifyFieldName fs x

optimize2 :: [[String]] -> [[String]] -> [[String]]
optimize2 [] _ = []
optimize2 ([]:xs) (y:ys) = ([]:(optimize2 xs (ys ++ [y])))
optimize2 ([x]:xs) (y:ys) = ([x]:(optimize2 xs (ys ++ [y])))
optimize2 (x:xs) (y:ys) = (t:(optimize2 xs (ys ++ [y])))
                where
                    t = if length m == 1 then m else x
                    m = filter f x
                    f n = all (== False) $ map (\p -> n `elem` p) ys


optimize :: [[String]] -> Int -> [[String]]
optimize xs 0 = xs
optimize xs i = if complete then xs else optimize (optimize2 cleaned cleaned) (i-1)
                where
                    complete = all (== True) $ map (\x -> length x == 1 || length x == 0) xs
                    singles = concat $ filter (\x -> length x == 1) xs
                    cleaned = map f xs
                    f [] = []
                    f [x] = [x]
                    f xs = filter (`notElem` singles) xs

day16A :: String -> IO ()
day16A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sum $ map (getErrorRate (getFields input)) $ getData input

day16B :: String -> IO ()
day16B i = do
        input <- parseInput parseAll i
        let columnNames = identifyFieldNames (getFields input) $ combine $ ((getSeatData input):(filter (\x -> 0 == getErrorRate (getFields input) x) $ getData input))
        putStrLn $ show $ product $ map (snd) $ filter f $ zip (map fx $ optimize columnNames 30) $ getSeatData input
        where
            f (c, d) = isPrefixOf "departure" c
            fx [] = "Err"
            fx (x:xs) = x
