module Day25 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Utils

parseAll :: Parser (Int, Int)
parseAll = do
        d1 <- read <$> many1 digit
        newline
        d2 <- read <$> many1 digit
        newline
        return (d1, d2)

getLoopSize :: Int -> Int -> Int -> Int
getLoopSize v s pubK =
                let
                    t = v * s
                    r = t `mod` 20201227
                in
                    if pubK == r
                    then 1
                    else 1 + getLoopSize r s pubK

loopTransform :: Int -> Int -> Int  -> Int
loopTransform 0 v _ = v
loopTransform l v s =
                let
                    t = v * s
                    r = t `mod` 20201227
                in
                    loopTransform (l-1) r s

day25A :: String -> IO ()
day25A i = do
        input <- parseInput parseAll i
        let l1 = getLoopSize 1 7 $ fst input
        let l2 = getLoopSize 1 7 $ snd input
        putStrLn $ show $ loopTransform l1 1 $ snd input
        putStrLn $ show $ loopTransform l2 1 $ fst input

day25B :: String -> IO ()
day25B i = do
        input <- parseInput parseAll i
        putStrLn $ show $ input
