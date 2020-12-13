module Main where
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.Maybe ( fromMaybe )
import Day1 (day1A, day1B)
import Day2 (day2A, day2B)
import Day3 (day3A, day3B)
import Day4 (day4A, day4B)
import Day5 (day5A, day5B)
import Day6 (day6A, day6B)
import Day7 (day7A, day7B)
import Day8 (day8A, day8B)
import Day9 (day9A, day9B)
import Day10 (day10A, day10B)
import Day11 (day11A, day11B)
import Day12 (day12A, day12B)
import Day13 (day13A, day13B)

data Flag
    = Day Int | IterB | Input String
      deriving Show

main :: IO ()
main = do
    (day, flag, file) <- getArgs >>= parse
    input <- loadInput file
    run day flag input

loadInput "stdin" = getContents
loadInput f = readFile f

run 1 'a' i = day1A i
run 1 'b' i = day1B i
run 2 'a' i = day2A i
run 2 'b' i = day2B i
run 3 'a' i = day3A i
run 3 'b' i = day3B i
run 4 'a' i = day4A i
run 4 'b' i = day4B i
run 5 'a' i = day5A i
run 5 'b' i = day5B i
run 6 'a' i = day6A i
run 6 'b' i = day6B i
run 7 'a' i = day7A i
run 7 'b' i = day7B i
run 8 'a' i = day8A i
run 8 'b' i = day8B i
run 9 'a' i = day9A i
run 9 'b' i = day9B i
run 10 'a' i = day10A i
run 10 'b' i = day10B i
run 11 'a' i = day11A i
run 11 'b' i = day11B i
run 12 'a' i = day12A i
run 12 'b' i = day12B i
run 13 'a' i = day13A i
run 13 'b' i = day13B i
run d o i = do
    hPutStrLn stderr ("Unknown day " ++ show d)
    exitWith (ExitFailure 1)

options :: [OptDescr Flag]
options =
    [ Option ['d']     ["day"]     (ReqArg day "DAY")       "day DAY"
    , Option ['b']     ["iter"]    (NoArg IterB)            "iteration b"
    , Option ['f']     ["file"]    (ReqArg Input "FILE")      "file FILE"
    ]


day = Day . read
inp = Input . fromMaybe "stdin"

parse :: [String] -> IO (Int, Char, String)
parse argv  =
    case getOpt Permute options argv of
        (o,n,[]) -> return (getDay o, getIter o, getFile o)
        (o,n,errs) -> do
            hPutStrLn stderr (show o)
            hPutStrLn stderr (concat errs ++ usageInfo header options)
            exitWith (ExitFailure 1)
    where header = "Usage: aoc [OPTION...] file..."


getDay :: [Flag] -> Int
getDay ((Day d):_) = d
getDay (_:fs) = getDay fs
getDay [] = 1

getIter :: [Flag] -> Char
getIter ((IterB):_) = 'b'
getIter (_:fs) = getIter fs
getIter [] = 'a'

getFile :: [Flag] -> String
getFile ((Input s):_) = tail s
getFile (_:fs) = getFile fs
getFile [] = "stdin"

