module Day11 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Math.Geometry.Grid
import Math.Geometry.Grid.OctagonalInternal
import Data.List (nub)
import Data.Maybe
import qualified Data.Vector as V

data Elem = Floor | OccupiedSeat | EmptySeat deriving (Show, Eq)

data Map = Map RectOctGrid (V.Vector (V.Vector Elem)) deriving (Show, Eq)

type Checker = Map -> (Int, Int) -> Bool

parseElem :: Parser Elem
parseElem = do
        c <- (char 'L' <|> char '#' <|> char '.')
        return $ case c of
            'L' -> EmptySeat
            '#' -> OccupiedSeat
            '.' -> Floor


parseRow :: Parser [Elem]
parseRow = do
        elems <- many1 parseElem
        newline
        return $ elems

parseAll :: Parser [[Elem]]
parseAll = do
        row <- many1 parseRow
        return row

parseInput :: String -> IO [[Elem]]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val



createMap :: (V.Vector (V.Vector Elem)) -> Map
createMap e = Map (rectOctGrid h w) e
    where
        h = V.length e
        w = V.length (V.head e)

createMapFromList :: [[Elem]] -> Map
createMapFromList e = createMap $ V.fromList $ map V.fromList e

getGrid :: Map -> RectOctGrid
getGrid (Map g _) = g

getFields :: Map -> V.Vector (V.Vector Elem)
getFields (Map _ f) = f

getSeatState :: (Int, Int) -> Map -> Elem
getSeatState (x, y) (Map _ es) = (es V.! y) V.! x

checkFree :: Map -> (Int, Int) -> Bool
checkFree m i = all (OccupiedSeat /=) $ map f $ neighbours g i
    where
        g = getGrid m
        f ie = getSeatState ie m

checkToBusy :: Map -> (Int, Int) -> Bool
checkToBusy m i = occ >= 4
      where
          occ = length $ filter (OccupiedSeat ==) $ map f $ neighbours g i
          g = getGrid m
          f ie = getSeatState ie m

processSeat :: Map -> (Int, Int) -> Checker -> Checker -> Elem
processSeat m g cF cB = case getSeatState g m of
                    Floor -> Floor
                    OccupiedSeat -> if cB m g then EmptySeat else OccupiedSeat
                    EmptySeat -> if cF m g then OccupiedSeat else EmptySeat

processRow :: Map -> Checker -> Checker -> Int -> V.Vector Elem
processRow m c1 c2 r = V.fromList $ map f $ [x| x <- [0..((snd s)-1)]]
    where
        g = getGrid m
        s = size g
        f c = processSeat m (c, r) c1 c2

simulate :: Map -> Checker -> Checker -> Map
simulate m c1 c2 = if changed then mu else simulate mu c1 c2
    where
        mu = (Map g u)
        changed = m == mu
        g = getGrid m
        s = size g
        u = V.fromList $ map (processRow m c1 c2) [y| y <- [0..((fst s)-1)]]

countOccupied :: Map -> Int
countOccupied (Map _ es) = V.length $ V.filter (OccupiedSeat ==) $ V.foldr1 (V.++) es

isDirectionFree :: Map -> RectOctGrid -> Direction RectOctGrid -> (Int, Int) -> Bool
isDirectionFree m g d i = case n of
                            Nothing -> True
                            Just t -> f (getSeatState t m) t
    where
        n = neighbour g i d
        f Floor t = isDirectionFree m g d t
        f EmptySeat _ = True
        f OccupiedSeat _ = False

directions = [North, West, South, East, Northwest, Northeast, Southeast, Southwest]

checkFree2 :: Map -> (Int, Int) -> Bool
checkFree2 m i = foldr1 (&&) $ map (isFree) directions
    where
        g = getGrid m
        isFree d = isDirectionFree m g d i

checkToBusy2 :: Map -> (Int, Int) -> Bool
checkToBusy2 m i = occ >= 5
      where
        occ = length $ filter (False ==) $ map (isFree) directions
        g = getGrid m
        isFree d = isDirectionFree m g d i

day11A :: String -> IO ()
day11A i = do
        input <- parseInput i
        putStrLn $ show $ countOccupied $ simulate (createMapFromList input) checkFree checkToBusy


day11B :: String -> IO ()
day11B i = do
        input <- parseInput i
        putStrLn $ show $ countOccupied $ simulate (createMapFromList input) checkFree2 checkToBusy2
