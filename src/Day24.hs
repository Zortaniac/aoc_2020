module Day24 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.List (nub, sort, group, partition)
import Utils

data Direction = E | SE | SW | W | NW | NE deriving (Show)

parseDirection :: Parser Direction
parseDirection = do
        c <- letter
        case c of
            'e' -> return E
            'w' -> return W
            'n' -> do
                    c2 <- letter
                    if c2 == 'w'
                    then return NW
                    else return NE
            's' -> do
                    c2 <- letter
                    if c2 == 'w'
                    then return SW
                    else return SE

parseRow :: Parser [Direction]
parseRow = do
        ds <- many1 parseDirection
        newline
        return ds

parseAll :: Parser [[Direction]]
parseAll = do
        ds <- many1 parseRow
        return ds

identifyTile :: (Int, Int) -> [Direction] -> (Int, Int)
identifyTile b [] = b
identifyTile (x, y) (E:xs) = identifyTile (x+1, y) xs
identifyTile (x, y) (W:xs) = identifyTile (x-1, y) xs
identifyTile (x, y) (NW:xs) = identifyTile (x-1, y+1) xs
identifyTile (x, y) (NE:xs) = identifyTile (x, y+1) xs
identifyTile (x, y) (SW:xs) = identifyTile (x, y-1) xs
identifyTile (x, y) (SE:xs) = identifyTile (x+1, y-1) xs

day24A :: String -> IO ()
day24A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter (True ==) $ map f $ group $ sort $ map (identifyTile (0, 0)) input
    where
        f x = (length x `mod` 2) == 1

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = [(x-1, y), (x-1, y+1), (x, y+1), (x+1, y), (x+1, y-1), (x, y-1)]

getsBlack :: [(Int, Int)] -> (Int, Int) -> Bool
getsBlack fs x = 2 == (length $ filter (\x -> x `elem` fs) $ getNeighbours x)

processBlackTile :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
processBlackTile fs x =
                    let
                        (b, w) = partition (\x -> x `elem` fs) $ getNeighbours x
                        c = length $ b
                    in
                    filter (getsBlack fs) w ++ if c == 0 || c > 2 then [] else [x]

flipTiles :: [(Int, Int)] -> [(Int, Int)]
flipTiles xs = nub $ foldl (++) [] $ map (processBlackTile xs) xs

flipTilesRounds :: Int -> [(Int, Int)] -> [(Int, Int)]
flipTilesRounds 0 a = a
flipTilesRounds i a = flipTilesRounds (i-1) $ flipTiles a

day24B :: String -> IO ()
day24B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ flipTilesRounds 100 $ map head $ filter f $ group $ sort $ map (identifyTile (0, 0)) input
    where
        f x = (length x `mod` 2) == 1
