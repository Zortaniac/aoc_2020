module Day23 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Foldable (toList)
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Array.IO
import Utils

parseAll :: Parser [Int]
parseAll = do
        d <- many1 (digitToInt <$> digit)
        newline
        return d

findNext :: Int -> Int -> [Int] -> Int
findNext 0 m xs = findNext m m xs
findNext i m xs = if elem i xs
                  then findNext (i-1) m xs
                  else i

play :: Int -> [Int] -> [Int]
play 0 xs = xs
play r (x:xs) =
            let (e3, xxs)   = splitAt 3 xs
                next        = findNext (x-1) (1 + length xs) e3
                (b, (n:ns)) = break (next == ) xxs
            in play (r-1) $ b ++ (n:e3) ++ ns ++ [x]

playS :: Int -> S.Seq Int -> S.Seq Int
playS 0 xs = xs
playS r sxs = playS (r-1) $ (S.|>) m x
        where
            x = sxs `S.index` 0
            xs = S.drop 4 sxs
            e3 = toList $ S.take 3 $ S.drop 1 sxs
            n = findNext (x-1) (length sxs) $ e3
            m = if xs `S.index` 0 > 9
                then m2
                else case S.elemIndexL n $ S.take 7 xs of
                        Nothing -> m2
                        Just k -> foldr (S.insertAt (k+1)) xs $ e3
            m2 = case S.elemIndexR n xs of
                    Nothing -> S.fromList []
                    Just k -> foldr (S.insertAt (k+1)) xs $ e3

playV :: Int -> Int -> V.Vector Int -> V.Vector Int
playV 0 _ xs = xs
playV r i xs =
            let e1          = xs V.!i
                e2          = xs V.!e1
                e3          = xs V.!e2
                nxt         = xs V.!e3
                n           = findNext (i-1) (V.length xs - 1) [e1, e2, e3]
                nOld        = xs V.!n
                u           = [(i, xs V.!e3), (n, e1), (e3, nOld)]
            in playV (r-1) nxt (xs V.// u)

day23A :: String -> IO ()
day23A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ concat $ map show $ f $ play 100 input
        where
            f (1:xs) = xs
            f (x:xs) = f $ xs ++ [x]

initArray :: IOArray Int Int -> [(Int, Int)] -> IO (IOArray Int Int)
initArray a [] = return a
initArray a ((i, v):xs) = do
                        writeArray a i v
                        initArray a xs

playIO :: Int -> Int -> IOArray Int Int -> IO (IOArray Int Int)
playIO 0 _ a = return a
playIO r i a = do
           e1 <- readArray a i
           e2 <- readArray a e1
           e3 <- readArray a e2
           nxt <- readArray a e3
           (min, max) <- getBounds a
           let n = findNext (i-1) max [e1, e2, e3]
           nOld <- readArray a n
           writeArray a i nxt
           writeArray a n e1
           writeArray a e3 nOld
           playIO (r-1) nxt a

day23B :: String -> IO ()
day23B i = do
        input <- parseInput parseAll i
        let mInput = input ++ [(1 + maximum input)..1000000]
        a <- newListArray (1, 1000000) [2..1000001] :: IO (IOArray Int Int)
        initArray a $ ((zip input (tail input ++ [1 + maximum input])) ++ [(last mInput, head input)])
        playIO 10000000 (head input) a
        a1 <- readArray a 1
        a2 <- readArray a a1
        putStrLn $ show $ a1 * a2
