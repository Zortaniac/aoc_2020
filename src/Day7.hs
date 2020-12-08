module Day7 where
import Data.Char
import Data.List ( nub, intersect )
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Data.Sort

data Bag = Bag String String deriving (Show, Eq)
data Rule = Rule Bag [(Int, Bag)] deriving (Show)

parseNoBag :: Parser [(Int, Bag)]
parseNoBag = do
        string "no other bags"
        return []

parseDigit :: Parser Int
parseDigit = do
        num <- many1 digit
        char ' '
        return $ read num

parseBag :: Parser Bag
parseBag = do
        c1 <- many1 letter
        char ' '
        c2 <- many1 letter
        char ' '
        string "bag"
        optional (char 's')
        return $ Bag c1 c2

parseBagList :: Parser [(Int, Bag)]
parseBagList = do
        bags <- many1 (pb)
        return bags
            where
                pb = do
                    num <- parseDigit
                    bag <- parseBag
                    optional (char ',' >> char ' ')
                    return (num, bag)

parseRule :: Parser Rule
parseRule = do
        bag <- parseBag
        char ' '
        string "contain"
        char ' '
        bags <- (parseNoBag <|> parseBagList)
        char '.'
        newline
        return $ Rule bag bags

parseAll :: Parser [Rule]
parseAll = do
        bags <- many1 parseRule
        return bags

parseInput :: String -> IO [Rule]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

canContain :: Bag -> Rule -> Bool
canContain b (Rule _ bs) = any (== b) $ map snd bs


findRule :: Bag -> [Rule] -> Rule
findRule b ((Rule x xs):rs) = if b == x then Rule x xs else findRule b rs

canContainRecursive :: Bag -> [Rule] -> Rule -> Bool
canContainRecursive b rs (Rule r bs) = if canContain b (Rule r bs)
                                then True
                                else or $ map t $ map snd bs
                            where
                                t x = canContainRecursive b rs $ findRule x rs

countBags :: Bag -> Rule -> [Rule] -> Int
countBags b (Rule _ []) rs = 1
countBags b (Rule _ xs) rs = 1 + (sum $ map (\x -> fst x * (countBags (snd x) (findRule (snd x) rs) rs)) xs)

day7A :: String -> IO ()
day7A i = do
        rules <- parseInput i
        putStrLn $ show $ length $ filter (== True) $ map (canContainRecursive (Bag "shiny" "gold") rules) rules

subBags (Rule _ x) = x

day7B :: String -> IO ()
day7B i = do
        rules <- parseInput i
        putStrLn $ show $ (countBags x (findRule x rules) rules) - 1
        where
            x = (Bag "shiny" "gold")

