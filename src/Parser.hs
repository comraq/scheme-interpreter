module Parser (readExpr) where

import Control.Monad (void)
import Control.Monad.Except
import Data.Array.ST
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import Data.Ratio
import Data.Complex
import Numeric (readHex, readOct, readInt, readFloat, readDec)
import Text.ParserCombinators.Parsec

import Definition
import LispError (LispError(..), Parsed)
import LispVector (vector)


------- The Public Parsing Function -------

readExpr :: String -> Parsed LispVal
readExpr input = case parse expr "lisp" input of
    Left err  -> throwError $ ParserErr err
    Right val -> return val

  where expr :: Parser LispVal
        expr = parseExpr <* eof


------- Parsers -------

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseNumber
        <|> parseChar
        <|> parseBool
        <|> parseString
        <|> parseAnyQuoted
        <|> parseAnyList
        <|> parseVector


------- Any Atom Parser -------

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> minusSymbol <|> satisfy (\c -> c /= '-' && c `elem` symbolChars)
    rest  <- many $ letter <|> digit <|> symbol
    return . LAtom $ first : rest

  where minusSymbol :: Parser Char
        minusSymbol = char '-' <* notFollowedBy digit


------- Bool Parser -------

parseBool :: Parser LispVal
parseBool = try $ do
  char '#'
  boolChar <- oneOf "tf"
  return . LBool $ case boolChar of
    't' -> True
    'f' -> False


------- Character Literal Parser -------

parseChar :: Parser LispVal
parseChar = try $ do
  string "#\\"
  chr <- characterName <|> character
  return $ LChar chr

  where
    delimiters :: String
    delimiters = " ()"

    character :: Parser Char
    character = anyChar <* lookAhead (void (oneOf delimiters) <|> eof)

    characterName :: Parser Char
    characterName = (string "space" <|> string "newline") >>= \chrName ->
      return $ case chrName of
        "space"   -> ' '
        "newline" -> '\n'


------- String Parser -------

parseString :: Parser LispVal
parseString = do
  char '"'
  strings <- many validString
  char '"'
  return . LString . concat $ strings

validString :: Parser String
validString = many1 (noneOf "\\\"") <|> escaped
  where
    escaped :: Parser String
    escaped =  char '\\' >>
      oneOf "\\\"ntr" >>=
        \x -> return $ case x of
          'n' -> "\n"
          't' -> "\t"
          'r' -> "\r"
          _   -> [x]


------- Number Parsers -------

parseNumber :: Parser LispVal
parseNumber = fmap LNumber (peekFirst >> parseSNumber <?> "Parse: Invalid LNumber")
  where
    peekFirst :: Parser ()
    peekFirst = void . try . lookAhead $ digit <|> (char '-' >> digit)

parseSNumber :: Parser SchemeNumber
parseSNumber = tryRational <|> tryComplex <|> noBase <|> try withBase
  where
    maybeNeg :: Num a => Parser (a -> a)
    maybeNeg = maybe id (const negate) <$> optionMaybe (char '-')

    toInt :: String -> Integer
    toInt = fst . head . readDec

    toDouble :: String -> Double
    toDouble = fst . head . readFloat

    getNumInt :: Parser Integer
    getNumInt = do
      mbNeg  <- maybeNeg
      digits <- many1 digit
      return . mbNeg . toInt $ digits

    getNumDouble :: Parser Double
    getNumDouble = do
      mbNeg  <- maybeNeg
      (a, b) <- many1 digit `sepByOnce` char '.'
      return . mbNeg . toDouble $ a ++ '.':b

    noBase :: Parser SchemeNumber
    noBase = SDouble <$> try getNumDouble <|> SInt <$> getNumInt

    withBase :: Parser SchemeNumber
    withBase = char '#' >> oneOf baseChars >>= getNumFromBaseChar

    baseChars :: String
    baseChars = "bodx"

    getNumFromBaseChar :: Char -> Parser SchemeNumber
    getNumFromBaseChar 'b' = SInt . fst . head . readBin   <$> many1 (oneOf binChars)
    getNumFromBaseChar 'o' = SInt . fst . head . readOct   <$> many1 octDigit
    getNumFromBaseChar 'd' = noBase
    getNumFromBaseChar 'x' = SInt . fst . head . readHex   <$> many1 hexDigit

    tryRational :: Parser SchemeNumber
    tryRational = try $ do
      (a, b) <- getNumInt `sepByOnce` char '/'
      return . SRational $ a % b

    tryComplex :: Parser SchemeNumber
    tryComplex = fmap SComplex $ rectComplex <|> polarComplex

    rectComplex :: Parser (Complex Double)
    rectComplex = try $ do
      rPart    <- getNum
      iPartNeg <- oneOf "+-" >>= \op -> return $ if op == '-'
                                          then negate
                                          else id

      iPart    <- iPartNeg <$> getNum
      char 'i'
      return $ rPart :+ iPart

    polarComplex :: Parser (Complex Double)
    polarComplex = try $ do
      mag   <- maybeNeg <*> getNumInt
      char '@'
      angle <- getNum
      return $ mkPolar (fromIntegral mag) angle

    getNum :: Parser Double
    getNum = try getNumDouble <|> fmap fromIntegral getNumInt


------- Quoted Parsers -------

parseAnyQuoted :: Parser LispVal
parseAnyQuoted = parseQuasiQuoted <|> parseQuoted

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ LList [LAtom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = between (string "`(") (char ')') quasiQuoted
  where
    quasiQuoted :: Parser LispVal
    quasiQuoted = LList . (LAtom "quasiquote":)
                  <$> ((unquoted <|> parseExpr) `sepBy` spaces1)

    unquoted :: Parser LispVal
    unquoted = LList . (LAtom "unquoted":) <$> do
                  char ','
                  unpackList <|> fmap (:[]) parseExpr

    unpackList :: Parser [LispVal]
    unpackList = do
      char '@'
      (LList vals) <- between (char '(') (char ')') parseList
      return $ LAtom "unpack":vals


------- List Parsers -------

parseAnyList :: Parser LispVal
parseAnyList = between (char '(') (char ')') anyList
  where
    anyList :: Parser LispVal
    anyList = try parseDottedList <|> parseList

parseList :: Parser LispVal
parseList = LList <$> parseExpr `sepEndBy` spaces1

parseDottedList :: Parser LispVal
parseDottedList =
  let head = parseExpr `endBy` spaces1
      tail = char '.' >> spaces1 >> parseExpr
  in  LDottedList <$> head <*> tail


------- Vector Parser -------

parseVector :: Parser LispVal
parseVector = between (string "#(") (char ')') $ do
  vals <- parseExpr `sepEndBy` spaces1
  return $ LVector (runSTArray $ vector vals)


------- Helper Parsers -------

binChars :: String
binChars = "01"

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: String -> [(Integer, String)]
readBin = readInt 2 (`elem` binChars) digitToInt

symbolChars :: String
symbolChars = "!$%&|*+/:<=?>@^_~-"

symbol :: Parser Char
symbol =  oneOf symbolChars

spaces1 :: Parser ()
spaces1 =  skipMany1 space

notSpace :: Parser Char
notSpace = satisfy (/=' ')

sepByOnce :: Parser a -> Parser sep -> Parser (a, a)
sepByOnce p sep = (,) <$> p <* sep <*> p
