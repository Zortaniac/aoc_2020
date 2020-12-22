module Day20 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (intersect, find, delete, group, sort, nub)
import Utils

data Pixel = Dot | Hash deriving (Show, Eq, Ord)
data Tile = Tile Int [[Pixel]] [[Pixel]] deriving (Show, Eq)


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

getPixels :: Tile -> [[Pixel]]
getPixels (Tile _ _ ps) = ps

matches :: Tile -> Tile -> Bool
matches t x = if getTileNum x == getTileNum t
              then False
              else (not . null . intersect (getBorder x)) (getBorder t)

day20A :: String -> IO ()
day20A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ product $ map head $ filter (\x -> length x == 2) $ group $ map getTileNum $ [x | x <- input, y <- input, matches x y]

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

genRotations :: Tile -> Int -> [Tile]
genRotations _ 0 = []
genRotations (Tile i b ps) c = [x] ++ [z] ++ genRotations (rotateTile x) (c-1) ++ genRotations (rotateTile z) (c-1)
        where
            x = (Tile i b ps)
            z = (Tile i b (map reverse ps))

assembleCol :: [Tile] -> Tile -> [Tile]
assembleCol ts (Tile i bs ps) = if length rotations == 0
                                then []
                                else [m] ++ (assembleCol tss m)
        where
            bottom = last ps
            rotations = foldr (++) [] $ filter (not . null) $ map f $ [a | a <- ts, matches a (Tile i bs ps)]
            f x = [a | a <- genRotations x 5, bottom == head (getPixels a)] -- , bottom == head ps
            m = head rotations
            tss = delete m ts

createTiledPicture :: [Tile] -> Tile -> [[Tile]]
createTiledPicture ts s = [col] ++ if length rotations == 0
                                   then rotations
                                   else createTiledPicture tss m
        where
            col = (s:assembleCol ts s)
            ci = map getTileNum col
            left :: [Pixel]
            left = map last $ getPixels s
            rotations = filter (not . null) $ map f $ [a | a <- ts, matches a s]
            f x = [a | a <- genRotations x 5, left == map head (getPixels a)]
            m = head $ head rotations
            tss :: [Tile]
            tss = filter (\(Tile x _ _) -> notElem x ci) ts


getPixelsWithoutBorder :: Tile -> [[Pixel]]
getPixelsWithoutBorder (Tile _ _ ps) = map (tail.init) $ (tail . init) ps

createPicture :: [[Tile]] -> [[Pixel]]
createPicture [] = []
createPicture (x:xs) = px ++ (createPicture xs)
        where
            px :: [[Pixel]]
            px = rot $ foldr1 (++) $ map getPixelsWithoutBorder x
            rot ([]:ps) = []
            rot ps = [map head ps] ++ (rot $ map tail ps)

findSeaMonstersL :: [[Pixel]] -> Int
findSeaMonstersL [] = 0
findSeaMonstersL ps = if length ps < 3 || (length . head) ps < 20
                     then 0
                     else c
                     + findSeaMonstersL (map tail ps)
         where
            c = if f then 1 else 0
            f = ps!!0!!18 == Hash && ps!!1!!0 == Hash && ps!!1!!5 == Hash && ps!!1!!6 == Hash && ps!!1!!11 == Hash
                && ps!!1!!12 == Hash && ps!!1!!17 == Hash && ps!!1!!18 == Hash && ps!!1!!19 == Hash
                && ps!!2!!1 == Hash && ps!!2!!4 == Hash && ps!!2!!7 == Hash && ps!!2!!10 == Hash && ps!!2!!13 == Hash
                && ps!!2!!16 == Hash

findSeaMonsters :: [[Pixel]] -> Int
findSeaMonsters [] = 0
findSeaMonsters ps = findSeaMonstersL ps + findSeaMonsters (tail ps)

rotatePixels :: [[Pixel]] -> [[[Pixel]]]
rotatePixels ps = [nps] ++ [rnps] ++ rotatePixels rnps ++ rotatePixels nps
        where
           nps = rot ps
           rnps = map reverse nps
           rot ([]:xs) = []
           rot xs = [map head xs] ++ (rot $ map tail xs)

countHashes :: [Pixel] -> Int
countHashes [] = 0
countHashes (Hash:xs) = 1 + countHashes xs
countHashes (_:xs) = countHashes xs


day20B :: String -> IO ()
day20B i = do
        input <- parseInput parseAll i
        let start = head $ head $ filter (\x -> length x == 2) $ group $ [x | x <- input, y <- input, matches x y]
        let rest = delete start input
        let picture = createPicture $ createTiledPicture rest $ ensureLeft rest $ ensureUpper rest start
        let hashCount = sum $ map countHashes picture
        putStrLn $ show $ (-) hashCount $ (*) 15 $ head $ dropWhile (0 == ) $ map findSeaMonsters $ rotatePixels picture
