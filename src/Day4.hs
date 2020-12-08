module Day4 where
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import System.Environment
import System.IO
import System.Exit

data Field = BirthYear Int
           | IssueYear Int
           | ExpirationYear Int
           | Height String
           | HairColor String
           | EyeColor String
           | PassportId String
           | CountryIt Int deriving (Show)

data Passport = Passport [Field] deriving (Show)

parseIntField :: Parser Field
parseIntField = do
        key <- string "byr"
           <|> string "iyr"
           <|> string "eyr"
           <|> string "cid"
        char ':'
        v <- many digit
        let val = read v
        return $ case key of
            "byr" -> BirthYear val
            "iyr" -> IssueYear val
            "eyr" -> ExpirationYear val
            "cid" -> CountryIt val

parseStringField ::Parser Field
parseStringField = do
        key <- try (string "hgt")
           <|> try (string "hcl")
           <|> string "ecl"
           <|> string "pid"
        char ':'
        val <- many (noneOf " \n")
        return $ case key of
            "hgt" -> Height val
            "hcl" -> HairColor val
            "ecl" -> EyeColor val
            "pid" -> PassportId val

parseField :: Parser Field
parseField = do
        field <- try parseIntField
         <|> try parseStringField
        space
        return field

parsePassport :: Parser Passport
parsePassport = do
         fields <- many parseField
         newline
         return $ Passport fields

parseAll :: Parser [Passport]
parseAll = do
        passports <- many parsePassport
        return passports

parseInput :: String -> IO [Passport]
parseInput input = case parse parseAll "(unknown)" input of
    Left err -> do
        hPutStrLn stderr $ show err
        exitWith (ExitFailure 1)
    Right val -> return val

hasBirthYear [] = False
hasBirthYear ((BirthYear _):xs) = True
hasBirthYear (x:xs) = hasBirthYear xs

hasValidBirthYear [] = False
hasValidBirthYear ((BirthYear by):xs) = by >= 1920 && by <= 2002
hasValidBirthYear (x:xs) = hasValidBirthYear xs

hasIssueYear [] = False
hasIssueYear ((IssueYear _):xs) = True
hasIssueYear (x:xs) = hasIssueYear xs

hasValidIssueYear [] = False
hasValidIssueYear ((IssueYear iy):xs) = iy >= 2010 && iy <= 2020
hasValidIssueYear (x:xs) = hasValidIssueYear xs

hasExpirationYear [] = False
hasExpirationYear ((ExpirationYear _):xs) = True
hasExpirationYear (x:xs) = hasExpirationYear xs

hasValidExpirationYear [] = False
hasValidExpirationYear ((ExpirationYear ey):xs) = ey >= 2020 && ey <= 2030
hasValidExpirationYear (x:xs) = hasValidExpirationYear xs

hasHeight [] = False
hasHeight ((Height _):xs) = True
hasHeight (x:xs) = hasHeight xs

hasValidHeight [] = False
hasValidHeight ((Height h):xs) = isValid (drop (length h - 2) h)
                            where
                                isValid "cm" = i >= 150 && i <= 193
                                isValid "in" = i >= 59 && i <= 76
                                isValid _ = False
                                i = read (take (length h - 2) h)
hasValidHeight (x:xs) = hasValidHeight xs

hasHairColor [] = False
hasHairColor ((HairColor _):xs) = True
hasHairColor (x:xs) = hasHairColor xs

hasValidHairColor [] = False
hasValidHairColor ((HairColor hc):xs) = hc!!0 == '#' && all isHexDigit (tail hc)
hasValidHairColor (x:xs) = hasValidHairColor xs

hasEyeColor [] = False
hasEyeColor ((EyeColor ec):xs) = elem ec ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
hasEyeColor (x:xs) = hasEyeColor xs

hasValidEyeColor [] = False
hasValidEyeColor ((EyeColor _):xs) = True
hasValidEyeColor (x:xs) = hasValidEyeColor xs

hasPassportId [] = False
hasPassportId ((PassportId _):xs) = True
hasPassportId (x:xs) = hasPassportId xs

hasValidPassportId [] = False
hasValidPassportId ((PassportId pId):xs) = length pId == 9 && all isDigit pId
hasValidPassportId (x:xs) = hasValidPassportId xs

hasRequiredFields :: Passport -> Bool
hasRequiredFields (Passport f) = hasBirthYear f
                    && hasIssueYear f
                    && hasExpirationYear f
                    && hasHeight f
                    && hasHairColor f
                    && hasEyeColor f
                    && hasPassportId f

isValid :: Passport -> Bool
isValid (Passport f) = hasValidBirthYear f
                    && hasValidIssueYear f
                    && hasValidExpirationYear f
                    && hasValidHeight f
                    && hasValidHairColor f
                    && hasValidEyeColor f
                    && hasValidPassportId f

day4A :: String -> IO ()
day4A i = do
        passports <- parseInput i
        putStrLn $ show $ length $ filter (== True) $ map hasRequiredFields passports

day4B :: String -> IO ()
day4B i = do
        passports <- parseInput i
        putStrLn $ show $ length $ filter isValid $ filter (hasRequiredFields) passports

