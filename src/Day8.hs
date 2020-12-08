module Day8 where
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit
import Data.Maybe

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int deriving (Show)

data Program = Program [Instruction] Int Int deriving (Show)

parseInt :: Parser Int
parseInt = do
        sign <- option '+' (char '+' <|> char '-')
        num <- many1 digit
        return $ case sign of
            '+' -> read num
            '-' -> -(read num)

parseInstruction :: Parser Instruction
parseInstruction = do
        instruction <- (string "nop" <|> string "acc" <|> string "jmp")
        char ' '
        num <- parseInt
        newline
        return $ case instruction of
            "nop" -> Nop num
            "acc" -> Acc num
            "jmp" -> Jmp num

parseAll :: Parser [Instruction]
parseAll = do
        instructions <- many1 parseInstruction
        return instructions

parseInput :: String -> IO [Instruction]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

runUntilRepeat :: Program -> [Int] -> Int
runUntilRepeat (Program i pc acc) di =
                    case pcN `elem` di of
                        True -> accN
                        False -> runUntilRepeat (Program i pcN accN) (pcN:di)
                    where
                        pcN = case i!!pc of
                                (Jmp i) -> pc + i
                                _ -> pc + 1
                        accN = case i!!pc of
                                (Acc a) -> acc + a
                                _ -> acc

runUntilRepeatMaybe :: Program -> [Int] -> Maybe Int
runUntilRepeatMaybe (Program i pc acc) di =
                    case pcN >= length i of
                        True -> Just accN
                        False -> case pcN `elem` di of
                            True -> Nothing
                            False -> runUntilRepeatMaybe (Program i pcN accN) (pcN:di)
                    where
                        pcN = case i!!pc of
                                (Jmp i) -> pc + i
                                _ -> pc + 1
                        accN = case i!!pc of
                                (Acc a) -> acc + a
                                _ -> acc

runSwitched :: [Instruction] -> Int -> Int
runSwitched it p =  case rs of
                        (Just acc) -> acc
                        Nothing -> runSwitched it (p+1)
                    where
                        rs = case it!!p of
                            (Acc _) -> Nothing
                            _ -> runUntilRepeatMaybe (Program its 0 0) []
                        its = replaceNth p (swp (it!!p)) it
                        swp n = case n of
                                    (Jmp i) -> Nop i
                                    (Nop i) -> Jmp i
                                    t -> t

day8A :: String -> IO ()
day8A i = do
        instructions <- parseInput i
        putStrLn $ show $ runUntilRepeat (Program instructions 0 0) []

day8B :: String -> IO ()
day8B i = do
        instructions <- parseInput i
        putStrLn $ show $ runSwitched instructions 0

