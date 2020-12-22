module Day19 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (isPrefixOf, stripPrefix)
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
processRuleList _ s [] = Just [s]
processRuleList _ "" _ = Nothing
processRuleList rm s (i:is) = case processRules rm i s of
                                Nothing -> Nothing
                                Just xs -> packMaybe $ foldr (++) [] $ catMaybes $ map (\ss -> processRuleList rm ss is) xs

processRules :: M.Map Int Rule -> Int -> String -> Maybe [String]
processRules rm ri s = case (M.!) rm ri of
                        (Terminal c) -> case stripPrefix c s of
                                            Nothing -> Nothing
                                            Just s -> Just [s]
                        (NonTerminal rs) -> packMaybe $ foldr (++) [] $ catMaybes $ map (processRuleList rm s) rs

matches :: (String, Maybe [String]) -> Bool
matches (s, Nothing) = False
matches (s, (Just v)) = any (s  == ) v

day19A :: String -> IO ()
day19A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter f $ map (processRules (fst input) 0) (snd input)
    where
        f Nothing = False
        f (Just xs) = any null xs

day19B :: String -> IO ()
day19B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter f $ map (processRules (newRules $ fst input) 0) (snd input)
    where
        newRules m = M.updateWithKey q 8 $ M.updateWithKey q 11 m
        q 8 _ = Just $ NonTerminal [[42], [42, 8]]
        q 11 _ = Just $ NonTerminal [[42, 31], [42, 11, 31]]
        f Nothing = False
        f (Just xs) = any null xs
