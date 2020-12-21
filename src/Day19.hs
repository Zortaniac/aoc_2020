module Day19 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Map as M
import Utils

data Rule = Terminal String | NonTerminal [[Int]] deriving (Show)

parseTerminal :: Parser (Int, Rule)
parseTerminal = do
        d <- read <$> many1 digit
        string ": \""
        c <- letter
        char '"'
        return $ (d, Terminal [c])

parseNum :: Parser Int
parseNum = do
        d <- read <$> many1 digit
        optionMaybe (char ' ')
        return d

parseNumList :: Parser [Int]
parseNumList = many1 parseNum

parseNonTerminal :: Parser (Int, Rule)
parseNonTerminal = do
        d <- read <$> many1 digit
        string ": "
        l <- sepBy parseNumList (string "| ")
        return $ (d, NonTerminal l)

parseRule :: Parser (Int, Rule)
parseRule = do
        r <- (try parseTerminal <|> parseNonTerminal)
        newline
        return r

parseText :: Parser String
parseText = do
        s <- many1 letter
        newline
        return s

parseAll :: Parser (M.Map Int Rule, [String])
parseAll = do
        rs <- many1 parseRule
        newline
        xs <- many1 parseText
        return (M.fromList rs, xs)

packMaybe [] = Nothing
packMaybe xs = Just xs

unpackMaybe Nothing = []
unpackMaybe (Just xs) = xs

processRuleList :: M.Map Int Rule -> String -> [Int] -> Maybe [String]
processRuleList _ _ [] = Nothing
processRuleList rm s (i:is) = case processRules rm i s of
                                Nothing -> Nothing
                                Just xs -> packMaybe $ foldr (++) [] $ map (\ss -> combine ss $ processRuleList rm (drop (length ss) s) is) xs
        where
            combine :: String -> Maybe [String] -> [String]
            combine s Nothing = [s]
            combine s (Just xs) = map (\ss -> s ++ ss) xs

processRules :: M.Map Int Rule -> Int -> String -> Maybe [String]
processRules rm ri s = case (M.!) rm ri of
                        (Terminal c) -> if isPrefixOf c s then Just [c] else Nothing
                        (NonTerminal rs) -> packMaybe $ foldr (++) [] $ catMaybes $ map (processRuleList rm s) rs


matches :: (String, Maybe [String]) -> Bool
matches (s, Nothing) = False
matches (s, (Just v)) = any (s  == ) v

day19A :: String -> IO ()
day19A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter matches $ zip (snd input) $ map (processRules (fst input) 0) (snd input)

combine :: String -> Maybe [String] -> [String]
combine s Nothing = []
combine s (Just xs) = map (\ss -> s ++ ss) xs

processRuleList2 :: M.Map Int Rule -> String -> [Int] -> Maybe [String]
processRuleList2 _ _ [] = Just []
processRuleList2 rm s (i:is) = case processRules2 rm i s of
                                Nothing -> Nothing
                                Just xs -> packMaybe $ foldr (++) [] $ map (\ss -> combine ss $ processRuleList2 rm (drop (length ss) s) is) xs


processRules2 :: M.Map Int Rule -> Int -> String -> Maybe [String]
processRules2 rm 8 s = case processRules2 rm 42 s of
                        Nothing -> Nothing
                        (Just xs) -> packMaybe $ xs ++ (foldr (++) [] $ map (\ss -> combine ss $ processRules2 rm 8 (drop (length ss) s)) xs)
processRules2 rm 11 s = case processRules2 rm 42 s of
                        Nothing -> processRules2 rm 31 s
                        (Just xs) -> packMaybe $ (foldr (++) [] $ map (\ss -> combine ss $ processRules2 rm 31 (drop (length ss) s)) xs) ++ (foldr (++) [] $ map (\ss -> combine ss $ processRules2 rm 11 (drop (length ss) s)) xs)
processRules2 rm ri s = case (M.!) rm ri of
                        (Terminal c) -> if isPrefixOf c s then Just [c] else Nothing
                        (NonTerminal rs) -> packMaybe $ foldr (++) [] $ catMaybes $ map (processRuleList2 rm s) rs

day19B :: String -> IO ()
day19B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter matches $ zip (snd input) $ map (processRules2 (fst input) 0) (snd input)
