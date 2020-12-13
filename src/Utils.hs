module Utils where
import System.Environment
import System.IO
import System.Exit
import Text.ParserCombinators.Parsec

parseInput :: Parser a -> String -> IO a
parseInput parseAll input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val
