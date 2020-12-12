module Day12 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import qualified Data.Vector as V

data Direction = North | East | West | South | Forward deriving (Show)

data Instruction = TurnLeft Int | TurnRight Int | Move Direction Int deriving (Show)

type Waypoint = (Int, Int)

parseDirection :: Parser Direction
parseDirection = do
        c <- oneOf "NSWEF"
        return $ case c of
            'N' -> North
            'S' -> South
            'E' -> East
            'W' -> West
            'F' -> Forward


parseMoveInstruction :: Parser Instruction
parseMoveInstruction = do
        d <- parseDirection
        i <- read <$> many1 digit
        newline
        return $ Move d i

parseTurnInstruction :: Parser Instruction
parseTurnInstruction = do
        d <- oneOf "LR"
        i <- read <$> many1 digit
        newline
        return $ case d of
            'R' -> TurnRight i
            'L' -> TurnLeft i

parseAll :: Parser [Instruction]
parseAll = do
        row <- many1 (parseTurnInstruction <|> parseMoveInstruction)
        return row

parseInput :: String -> IO [Instruction]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

move (Move West i) _ = (-i, 0)
move (Move East i) _ = (i, 0)
move (Move South i) _ = (0, -i)
move (Move North i) _ = (0, i)
move (Move Forward i) d = case d of
                                North -> (0, i)
                                South -> (0, -i)
                                East -> (i, 0)
                                West -> (-i, 0)

turnLeft d = case d of
                 East -> North
                 North -> West
                 West -> South
                 South -> East

turnRight d = case d of
                 East -> South
                 South -> West
                 West -> North
                 North -> East

turn (TurnLeft 0) d = d
turn (TurnLeft i) d = turn (TurnLeft (i-90)) (turnLeft d)
turn (TurnRight 0) d = d
turn (TurnRight i) d = turn (TurnRight (i-90)) (turnRight d)

isMoveInstruction (Move _ _) = True
isMoveInstruction _ = False

simulate [] _ = []
simulate (i:is) d = if isMoveInstruction i
                    then (move i d):(simulate is d)
                    else simulate is (turn i d)

sum1 [] = (0, 0)
sum1 ((x, y):xs) = (x + fst t, y + snd t)
        where
            t = sum1 xs

absSum (x, y) = abs x + abs y

day12A :: String -> IO ()
day12A i = do
        input <- parseInput i
        putStrLn $ show $ absSum $ sum1 $ simulate input East

moveWP (Move West i) (x, y) = (x-i, y)
moveWP (Move East i) (x, y) = (x+i, y)
moveWP (Move South i) (x, y) = (x, y-i)
moveWP (Move North i) (x, y) = (x, y+i)

moveShip i (xw, yw) = (xw*i, yw*i)

turnWP (TurnLeft 0) (x, y) = (x, y)
turnWP (TurnLeft i) (x, y) = turnWP (TurnLeft (i-90)) (-y, x)
turnWP (TurnRight 0) (x, y) = (x, y)
turnWP (TurnRight i) (x, y) = turnWP (TurnRight (i-90)) (y, -x)

simulateShipMove (Move Forward i) wp = moveShip i wp
simulateShipMove _ _ = (0, 0)

simulateWayPointMove (Move Forward _) wp = wp
simulateWayPointMove i wp = moveWP i wp

simulate2 [] _ = []
simulate2 (i:is) wp = if isMoveInstruction i
                    then (simulateShipMove i wp):(simulate2 is (simulateWayPointMove i wp))
                    else simulate2 is (turnWP i wp)

day12B :: String -> IO ()
day12B i = do
        input <- parseInput i
        putStrLn $ show $ absSum $ sum1 $  simulate2 input (10, 1)
