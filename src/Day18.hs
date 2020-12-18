module Day18 where
import Text.Parsec.Char
import Text.ParserCombinators.Parsec
import Utils

data Term = IntTerm Int | ExprTerm Expr deriving (Show)
data Operation = AddOp | MulOp deriving (Show)
data Expr = Expr Operation Term Term deriving (Show)

parseIntTerm :: Parser Term
parseIntTerm = do
        d <- read <$> many1 digit
        return $ IntTerm d

parseSubTerm :: Parser Term
parseSubTerm = do
        char '('
        t1 <- parseTerm
        e <- parseExpr t1
        return $ e

parseTerm :: Parser Term
parseTerm = do
        (parseIntTerm <|> parseSubTerm)

parseOperation :: Parser Operation
parseOperation = do
        o <- (char '+' <|> char '*')
        return $ case o of
            '+' -> AddOp
            '*' -> MulOp

parseExpr :: Term -> Parser Term
parseExpr t1 = do
        mN <- optionMaybe (newline <|> char ')')
        case mN of
            Just _ -> return t1
            Nothing -> do
                char ' '
                o <- parseOperation
                char ' '
                t2 <- parseTerm
                parseExpr (ExprTerm (Expr o t1 t2))

parseRow :: Parser Term
parseRow = do
        t1 <- parseTerm
        e <- parseExpr t1
        return e

parseAll :: Parser [Term]
parseAll = do
        rs <- many1 parseRow
        return rs

compute :: Term -> Int
compute (IntTerm t) = t
compute (ExprTerm e) = case e of
                        (Expr AddOp t1 t2) -> (compute t1) + (compute t2)
                        (Expr MulOp t1 t2) -> (compute t1) * (compute t2)


day18A :: String -> IO ()
day18A i = do
        input <- parseInput parseAll i
        putStrLn $ show $ sum $ map compute input

parseSubTerm2 :: Parser Term
parseSubTerm2 = do
        char '('
        t1 <- parseTerm2
        e <- parseExpr2 t1
        char ')'
        return e

parseTerm2 :: Parser Term
parseTerm2 = do
        (parseIntTerm <|> parseSubTerm2)

parseOperation2 :: Parser Operation
parseOperation2 = do
        o <- (char '+' <|> char '*')
        return $ case o of
            '+' -> AddOp
            '*' -> MulOp


parseTermExpr :: Parser Term
parseTermExpr = do
            t1 <- parseTerm2
            notFollowedBy (char ' ' >> (char '+' <|> char '*'))
            return t1

parsePlusExpr :: Term -> Parser Term
parsePlusExpr t1 = do
            char ' '
            char '+'
            char ' '
            t2 <- parseTerm2
            eM <- optionMaybe $ parseExpr2 $ ExprTerm (Expr AddOp t1 t2)
            return $ case eM of
                Just e -> e
                Nothing ->ExprTerm (Expr AddOp t1 t2)

parseMulExpr :: Term -> Parser Term
parseMulExpr t1 = do
            char ' '
            char '*'
            char ' '
            t2 <- parseTerm2
            eM <- optionMaybe $ parseExpr2 t2
            return $ case eM of
                Just e -> ExprTerm (Expr MulOp t1 e)
                Nothing ->ExprTerm (Expr MulOp t1 t2)


parseExpr2 :: Term -> Parser Term
parseExpr2 t1 = try (parsePlusExpr t1) <|> parseMulExpr t1

parseRow2 :: Parser Term
parseRow2 = do
        t1 <- parseTerm2
        e <- parseExpr2 t1
        newline
        return e

parseAll2 :: Parser [Term]
parseAll2 = do
        rs <- many1 parseRow2
        return rs

day18B :: String -> IO ()
day18B i = do
        input <- parseInput parseAll2 i
        putStrLn $ show $ sum $ map compute input
