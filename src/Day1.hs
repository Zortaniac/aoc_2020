module Day1 where
import Data.List ( find )
import System.IO

day1A :: String -> IO ()
day1A i = do
        let l = lines i
        let s = map read l
        let c = cartProd s
        case find (\(x, y) -> x+y == 2020) c of
            Just (x, y) -> putStrLn . show $ x * y
            Nothing -> hPutStrLn stderr "Error"

day1B :: String -> IO ()
day1B i = do
        let l = lines i
        let s = map read l
        let c = cartProd3 s
        case find (\(x, y, z) -> x+y+z == 2020) c of
            Just (x, y, z) -> putStrLn . show $ x * y * z
            Nothing -> hPutStrLn stderr "Error"

cartProd (x:xs) = [(x,y) | y <- xs] ++ (cartProd xs)
cartProd [] = []

cartProd3 (x:xs) = [(x,y,z) | y <- (xs), z <- (tail xs)] ++ (cartProd3 xs)
cartProd3 [] = []
