module Day14 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.Word (Word64)
import Data.Bits ((.|.), (.&.), xor)
import Utils


data Mask = Mask Int Int String deriving (Show)
data MemAssign = MemAssign Int Int deriving (Show)
data InstructionBlock = InstructionBlock Mask [MemAssign] deriving (Show)


parseMask :: Parser Mask
parseMask = do
        string "mask = "
        c <- many1 (char '0' <|> char '1' <|> char 'X')
        newline
        return $ Mask (convertToOrMask c) (convertToAndMask c) (reverse c)

convertToOrMask :: String -> Int
convertToOrMask s = foldr1 (.|.) $ f $ zip [2^n | n <- [0..]] $ reverse s
        where
            f [] = [0]
            f ((v, '1'):xs) = [v] ++ f xs
            f ((v, _):xs) = f xs


convertToAndMask :: String -> Int
convertToAndMask s = foldr1 (.|.) $ f $ zip [2^n | n <- [0..]] $ reverse s
        where
            f [] = [0]
            f ((v, '0'):xs) = f xs
            f ((v, _):xs) = [v] ++ f xs

parseMemAssign :: Parser MemAssign
parseMemAssign = do
        string "mem["
        i <- read <$> many1 digit
        string "] = "
        v <- read <$> many1 digit
        newline
        return $ MemAssign i v

parseInstructionBlock :: Parser InstructionBlock
parseInstructionBlock = do
        mask <- parseMask
        memAssigns <- many1 (try parseMemAssign)
        return $ InstructionBlock mask  memAssigns

parseAll :: Parser [InstructionBlock]
parseAll = do
        instr <- many1 parseInstructionBlock
        return instr

run :: [InstructionBlock] -> [(Int, Int)]
run [] = []
run ((InstructionBlock (Mask orM andM _) a):xs) = t ++ run xs
        where
            t = map f a
            f (MemAssign i v) = (i, (v .|. orM) .&. andM)

sumFiltered :: [Int] -> [(Int, Int)] -> Int
sumFiltered _ [] = 0
sumFiltered is ((i, v):xs) = sumFiltered (i:is) xs + if i `elem` is then 0 else v

day14A :: String -> IO ()
day14A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sumFiltered [] $ reverse $ run input

getAllAddresses :: Int -> [(Char, Int)] -> [Int]
getAllAddresses i [] = [i]
getAllAddresses i (('X', m):s) = getAllAddresses i s ++ getAllAddresses ix s
        where
            ix = i `xor` m
getAllAddresses i (_:s) = getAllAddresses i s

run2 :: [InstructionBlock] -> [(Int, Int)]
run2 [] = []
run2 ((InstructionBlock (Mask orM andM ms) a):xs) = t ++ run2 xs
        where
            t = foldr (++) [] $ map f a
            f (MemAssign i v) = map (\xi -> (xi, v)) $ getAllAddresses (i .|. orM) $ zip ms [2^n | n <- [0..]]


day14B :: String -> IO ()
day14B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sumFiltered [] $ reverse $ run2 input
