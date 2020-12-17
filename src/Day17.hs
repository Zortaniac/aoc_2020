module Day17 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import qualified Data.Map as M
import qualified Data.Vector as V
import Utils

data CubeState = Active | Inactive deriving (Show, Eq)

type Lookup = M.Map (Int, Int, Int) CubeState
type Dimensions = ((Int, Int), (Int, Int), (Int, Int))

type Lookup4 = M.Map (Int, Int, Int, Int) CubeState
type Dimensions4 = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))

parseCubeState :: Parser CubeState
parseCubeState = do
        c <- (char '.' <|> char '#')
        return $ case c of
            '#' -> Active
            '.' -> Inactive

parseRow :: Parser [CubeState]
parseRow = do
        s <- many1 parseCubeState
        newline
        return s

parseAll :: Parser [[CubeState]]
parseAll = do
        rs <- many1 parseRow
        return rs

getActiveNeighbourCount :: Lookup -> (Int, Int, Int) -> Int
getActiveNeighbourCount m (xc, yc, zc) = length . filter (Active ==) $ map f $ [(x, y, z) | x <- [(xc-1)..(xc+1)], y <- [(yc-1)..(yc+1)], z <- [(zc-1)..(zc+1)]]
    where
        f c = case (M.lookup) c m of
                Just Active -> Active
                otherwise -> Inactive

shouldActivate :: Lookup -> (Int, Int, Int) -> Bool
shouldActivate m c = 3 == getActiveNeighbourCount m c

shouldStayActivate :: Lookup -> (Int, Int, Int) -> Bool
shouldStayActivate m c = cnt == 3 || cnt == 4
    where
       cnt = getActiveNeighbourCount m c

determineNewState :: Lookup -> (Int, Int, Int) -> CubeState
determineNewState m c = case (M.lookup) c m of
                            Just Active -> if shouldStayActivate m c then Active else Inactive
                            otherwise -> if shouldActivate m c then Active else Inactive

boot :: Int -> Dimensions -> Lookup -> Lookup
boot 0 _ m = m
boot c ((xs, xe), (ys, ye), (zs, ze)) m = boot (c-1) du mu
    where
        du = ((xs-1, xe+1), (ys-1, ye+1), (zs-1, ze+1))
        mu = M.fromList $ map f [(x, y, z) | x <- [xs..xe], y <- [ys..ye], z <- [zs..ze]]
        f c = (c, determineNewState m c)

day17A :: String -> IO ()
day17A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter (Active == ) $ M.elems $ boot 6 (d input) $ M.fromList $ concat $ map (\l -> zip (fst l) (snd l)) $ zip il input
        where
            il = map (\y -> [(x, y, 0) | x <- [0..]]) [0..]
            d i = ((-1, length $ head i), (-1, length i), (-1, 1))

getActiveNeighbourCount4 :: Lookup4 -> (Int, Int, Int, Int) -> Int
getActiveNeighbourCount4 m (xc, yc, zc, wc) = length . filter (Active ==) $ map f d
    where
        d = [(x, y, z, w) | x <- [(xc-1)..(xc+1)], y <- [(yc-1)..(yc+1)], z <- [(zc-1)..(zc+1)], w <- [(wc-1)..(wc+1)]]
        f c = case (M.lookup) c m of
                Just Active -> Active
                otherwise -> Inactive

shouldActivate4 :: Lookup4 -> (Int, Int, Int, Int) -> Bool
shouldActivate4 m c = 3 == getActiveNeighbourCount4 m c

shouldStayActivate4 :: Lookup4 -> (Int, Int, Int, Int) -> Bool
shouldStayActivate4 m c = cnt == 3 || cnt == 4
    where
       cnt = getActiveNeighbourCount4 m c

determineNewState4 :: Lookup4 -> (Int, Int, Int, Int) -> CubeState
determineNewState4 m c = case (M.lookup) c m of
                            Just Active -> if shouldStayActivate4 m c then Active else Inactive
                            otherwise -> if shouldActivate4 m c then Active else Inactive

boot4 :: Int -> Dimensions4 -> Lookup4 -> Lookup4
boot4 0 _ m = m
boot4 c ((xs, xe), (ys, ye), (zs, ze), (ws, we)) m = boot4 (c-1) du mu
    where
        du = ((xs-1, xe+1), (ys-1, ye+1), (zs-1, ze+1), (ws-1, we+1))
        mu = M.fromList $ map f [(x, y, z, w) | x <- [xs..xe], y <- [ys..ye], z <- [zs..ze], w <- [ws..we]]
        f c = (c, determineNewState4 m c)

day17B :: String -> IO ()
day17B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ length $ filter (Active == ) $ M.elems $ boot4 6 (d input) $ M.fromList $ concat $ map (\l -> zip (fst l) (snd l)) $ zip il input
        where
            il = map (\y -> [(x, y, 0, 0) | x <- [0..]]) [0..]
            d i = ((-1, length $ head i), (-1, length i), (-1, 1), (-1, 1))
