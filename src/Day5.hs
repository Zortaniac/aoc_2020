module Day5 where
import Data.Char
import Data.List ( find )
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Data.Sort

data Seat = R | L deriving (Show)
data Row = B | F  deriving (Show)

data BoardingPass = BoardingPass [Row] [Seat] deriving (Show)

split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList

parseRow :: Parser Row
parseRow = do
        k <- char 'B'
           <|> char 'F'
        return $ case k of
            'B' -> B
            'F' -> F

parseSeat ::Parser Seat
parseSeat = do
        key <- char 'R'
           <|> char 'L'
        return $ case key of
            'R' -> R
            'L' -> L

parseBoardingPass :: Parser BoardingPass
parseBoardingPass = do
         rows <- many parseRow
         seats <- many parseSeat
         newline
         return $ BoardingPass rows seats

parseAll :: Parser [BoardingPass]
parseAll = do
        passes <- many parseBoardingPass
        return passes

parseInput :: String -> IO [BoardingPass]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

calcRow :: [Row] -> [Int] -> Int
calcRow _ [x] = x
calcRow (F:rs) xs = calcRow rs $ fst $ split xs
calcRow (B:rs) xs = calcRow rs $ snd $ split xs

calcSeat :: [Seat] -> [Int] -> Int
calcSeat _ [x] = x
calcSeat (L:rs) xs = calcSeat rs $ fst $ split xs
calcSeat (R:rs) xs = calcSeat rs $ snd $ split xs

calcSeatId :: BoardingPass -> Int
calcSeatId (BoardingPass rows seats) = row * 8 + seat
                                where
                                    row = calcRow rows [0..127]
                                    seat = calcSeat seats [0..7]

day5A :: String -> IO ()
day5A i = do
        passes <- parseInput i
        putStrLn $ show $ maximum $ map calcSeatId passes

day5B :: String -> IO ()
day5B i = do
        passes <- parseInput i
        let seatIds = sort $ map calcSeatId passes
        let first = head seatIds
        case find (\x -> (fst x) /= (snd x)) $ zip seatIds [first..] of
            Just x -> putStrLn $ show $ snd x
            Nothing -> putStrLn "Error"

