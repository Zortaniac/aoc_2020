module Day21 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (groupBy, sort, intersect, nub, (\\), intercalate)
import Utils

parseWord :: Parser String
parseWord = do
        s <- many1 letter
        try (char ' ')
        return s

parseAllergens :: Parser [String]
parseAllergens = do
        string "(contains "
        xs <- sepBy (many1 letter) (string ", ")
        char ')'
        return xs

parseRow :: Parser ([String], [String])
parseRow = do
        ingredients <- many1 parseWord
        allergens <- option [] parseAllergens
        newline
        return (ingredients, allergens)

parseAll :: Parser [([String], [String])]
parseAll = many1 parseRow


cnt :: [([String], [String])] -> [String] -> Int
cnt [] _ = 0
cnt (x:xs) is = cnt xs is + (length $ intersect (fst x) is)

day21A :: String -> IO ()
day21A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ cnt input $ ingredients input \\ (foldr1 (++) $ map snd $ map f3 $ groupBy f2 $ sort $ foldr1 (++) $ map f1 input)
    where
        f1 (xs, i) = map (\x -> (x, xs)) i
        f2 x y = fst x == fst y
        f3 :: [(String, [String])] -> (String, [String])
        f3 xs = (fst (head xs), foldr1 intersect $ map snd xs)
        ingredients xs = nub $ foldr1 (++) $ map fst xs

optimize :: [(String, [String])] -> [(String, String)]
optimize [] = []
optimize ((a, i):xs) = if isSingle then ((a, ingredient): optimize cleaned) else optimize (xs ++ [(a, i)])
                where
                    isSingle = length i == 1
                    ingredient = head i
                    cleaned = map f xs
                    f (a, is) = (a, filter (ingredient /=) is)

day21B :: String -> IO ()
day21B i = do
        input <- parseInput parseAll i
        putStrLn $ intercalate "," $ map snd $ sort $ optimize $ map f3 $ groupBy f2 $ sort $ foldr1 (++) $ map f1 input
    where
        f1 (xs, i) = map (\x -> (x, xs)) i
        f2 x y = fst x == fst y
        f3 :: [(String, [String])] -> (String, [String])
        f3 xs = (fst (head xs), foldr1 intersect $ map snd xs)
