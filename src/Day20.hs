module Day20 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (intersect, find)
import Utils

data Pixel = Dot | Hash deriving (Show, Eq)
data Tile = Tile Int [[Pixel]] [[Pixel]] deriving (Show)


parsePixel :: Parser Pixel
parsePixel = do
        c <- (char '.' <|> char '#')
        return $ case c of
            '.' -> Dot
            '#' -> Hash

parsePixelRow :: Parser [Pixel]
parsePixelRow = do
        p <- many1 parsePixel
        newline
        return p

parseTile :: Parser Tile
parseTile = do
        string "Tile "
        i <- read <$> many1 digit
        char ':'
        newline
        pixels <- many1 parsePixelRow
        optionMaybe newline
        return $ Tile i (calculateBorder pixels) pixels


parseAll :: Parser [Tile]
parseAll = many1 parseTile

calculateBorder :: [[Pixel]] -> [[Pixel]]
calculateBorder ps = border ++ map reverse border
        where
            border = [t, b, l, r]
            t = head ps
            b = last ps
            l = map head ps
            r = map last ps

getBorder :: Tile -> [[Pixel]]
getBorder (Tile _ b _) = b

getTileNum :: Tile -> Int
getTileNum (Tile i _ _) = i

matches :: Tile -> Tile -> Bool
matches t x = if getTileNum x == getTileNum t
              then False
              else (not . null . intersect (getBorder x)) (getBorder t)

findCorners :: [Tile] -> [Tile] -> [Tile]
findCorners [] _ = []
findCorners (x:xs) ts = if c == 2 then (x: findCorners xs ts) else findCorners xs ts
        where
            c = length $ filter (True == ) $ map (matches x) ts

day20A :: String -> IO ()
day20A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ product $ map getTileNum $ findCorners input input

ensureUpper :: [Tile] -> Tile -> Tile
ensureUpper ts (Tile i b ps) = if t then (Tile i b (reverse ps)) else (Tile i b ps)
        where
            bt = [head ps] ++ [reverse (head ps)]
            t = any (True == ) $ map f ts
            f x = if i == getTileNum x
              then False
              else (not . null . intersect (getBorder x)) bt

ensureLeft :: [Tile] -> Tile -> Tile
ensureLeft ts (Tile i b ps) = if t then (Tile i b (map reverse ps)) else (Tile i b ps)
        where
            first = map head ps
            bt = [first] ++ [reverse first]
            t = any (True == ) $ map f ts
            f x = if i == getTileNum x
              then False
              else (not . null . intersect (getBorder x)) bt

rotateTile :: Tile -> Tile
rotateTile (Tile i b ps) = (Tile i b (rot ps))
        where
            rot ([]:xs) = []
            rot xs = [map head xs] ++ (rot $ map tail xs)

ensureRotation :: [Pixel] -> Tile -> Tile
ensureRotation ps (Tile i b tps) = if f || fr
                                   then t
                                   else ensureRotation ps $ rotateTile (Tile i b tps)
        where
           first = map head tps
           f = ps == first
           fr = ps == reverse first
           t = if f
             then (Tile i b tps)
             else (Tile i b (reverse tps))


ensureRotation2 :: [Pixel] -> Tile -> Tile
ensureRotation2 ps (Tile i b tps) = if f || fr
                                   then t
                                   else ensureRotation2 ps $ rotateTile (Tile i b tps)
        where
           first = head tps
           f = ps == first
           fr = ps == reverse first
           t = if f
             then (Tile i b tps)
             else (Tile i b (map reverse tps))

assembleRow :: [Tile] -> Tile -> [Tile]
assembleRow ts (Tile i b ps) = case m of
                                Just t -> [t] ++ assembleRow (filter (\x -> getTileNum x /= getTileNum t) ts) t
                                Nothing -> []
        where
            left = map last ps
            bt = [left] ++ [reverse left]
            m = case find f ts of
                    Just t -> Just (ensureRotation left t)
                    Nothing -> Nothing
            f x = if i == getTileNum x
              then False
              else (not . null . intersect (getBorder x)) bt

createPicture :: [Tile] -> Tile -> [[Tile]]
createPicture [] _ = []
createPicture ts (Tile i b ps)  = [row]  ++ case n of
                                                   Just t -> createPicture (filter (\x -> getTileNum x /= getTileNum t && getTileNum x `notElem` ri) ts) t
                                                   Nothing -> []
        where
            row = ((Tile i b ps):assembleRow ts (Tile i b ps))
            ri = map getTileNum row
            lst = last ps
            bt = [lst] ++ [reverse lst]
            n = case find f ts of
                    Just x -> Just (ensureRotation2 lst x)
                    Nothing -> Nothing
            f x = if i == getTileNum x
              then False
              else (not . null . intersect (getBorder x)) bt

day20B :: String -> IO ()
day20B i = do
        i <- parseInput parseAll i
        let start = head $ findCorners i i
        let input = filter (\t -> getTileNum start /= getTileNum t) i
        putStrLn $ show $ take 10 $ createPicture input $ ensureLeft input $ ensureUpper input start
