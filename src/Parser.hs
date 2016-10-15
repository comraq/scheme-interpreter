module Parser (readExpr, readExprList) where

import Control.Monad.Except
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import Data.Ratio
import Data.Complex
import Numeric (readHex, readOct, readInt, readFloat, readDec)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

import Definition
import LispVector (vector)


------- The Exposed Parsing Function -------

readOrThrow :: Parser a -> String -> Evaled a
readOrThrow parser input = case parse parser "lisp" input of
  Left err  -> throwError $ ParserErr err
  Right val -> return val

readExpr :: String -> Evaled LispVal
readExpr = readOrThrow $ parseExpr <* eof

readExprList :: String -> Evaled [LispVal]
readExprList = readOrThrow $ endBy parseExpr spaces


------- Parsers -------

parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseAtom
        <|> parseChar
        <|> parseBool
        <|> parseString
        <|> parseAnyQuoted
        <|> parseAnyList
        <|> parseVector


------- Any Atom Parser -------

parseAtom :: Parser LispVal
parseAtom = try $ do
  atom <- identifier
  if atom == "."
    then pzero
    else return $ LAtom atom


------- Bool Parser -------

parseBool :: Parser LispVal
parseBool = try $ do
  char '#'
  boolChar <- oneOf "tf"
  return . LBool $ case boolChar of
    't' -> True
    _   -> False


------- Character Literal Parser -------

parseChar :: Parser LispVal
parseChar = try (string "#\\") >> fmap LChar character
  where
    character :: Parser Char
    character = do
      ch   <- anyChar
      rest <- many letter
      case ch:rest of
        "altmode"   -> return '\ESC'
        "backnext"  -> return '\US'
        "backspace" -> return '\BS'
        "call"      -> return '\SUB'
        "delete"    -> return '\DEL'
        "escape"    -> return '\ESC'
        "linefeed"  -> return '\LF'
        "newline"   -> return '\n'
        "null"      -> return '\NUL'
        "page"      -> return '\FF'
        "return"    -> return '\CR'
        "rubout"    -> return '\DEL'
        "space"     -> return ' '
        "tab"       -> return '\t'
        [c]         -> return c
        _           -> pzero


------- String Parser -------

parseString :: Parser LispVal
parseString = do
  char '"'
  chars <- many $ validStringChar <|> noneOf "\""
  char '"'
  return $ LString chars

validStringChar :: Parser Char
validStringChar = do
  char '\\'
  c <- oneOf "\\\"abnrt"
  return $ case c of
    'a' -> '\a'
    'b' -> '\b'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> c


------- Number Parsers -------

parseNumber :: Parser LispVal
parseNumber = fmap LNumber . try $ parseSNumber <* notFollowedBy (letter <|> digit <|> symbol)

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
    noBase = fmap SDouble (try getNumDouble) <|> fmap SInt (try getNumInt)

    withBase :: Parser SchemeNumber
    withBase = char '#' >> oneOf baseChars >>= getNumFromBaseChar

    baseChars :: String
    baseChars = "bodx"

    getNumFromBaseChar :: Char -> Parser SchemeNumber
    getNumFromBaseChar 'b' = SInt . fst . head . readBin <$> many1 (oneOf binChars)
    getNumFromBaseChar 'o' = SInt . fst . head . readOct <$> many1 octDigit
    getNumFromBaseChar 'd' = noBase
    getNumFromBaseChar 'x' = SInt . fst . head . readHex <$> many1 hexDigit

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
parseAnyQuoted =   parseQuasiQuoted
               <|> parseUnquoteSpliced
               <|> parseUnquoted
               <|> parseQuoted

parseQuoted :: Parser LispVal
parseQuoted = do
  lexeme $ char '\''
  x <- parseExpr
  return $ LList [LAtom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  lexeme $ char '`'
  expr <- parseExpr
  return $ LList [LAtom "quasiquote", expr]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  lexeme $ char ','
  expr <- parseExpr
  return $ LList [LAtom "unquote", expr]

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do
  try . lexeme $ string ",@"
  expr <- parseExpr
  return $ LList [LAtom "unquote-splicing", expr]


------- List Parsers -------

parseAnyList :: Parser LispVal
parseAnyList = parens anyList
  where
    anyList :: Parser LispVal
    anyList = try parseDottedList <|> parseList

parseList :: Parser LispVal
parseList = LList <$> parseExpr `sepEndBy` whiteSpace

parseDottedList :: Parser LispVal
parseDottedList =
  let hd = parseExpr `endBy` whiteSpace
      tl = dot >> parseExpr
  in  LDottedList <$> hd <*> tl


------- Vector Parser -------

parseVector :: Parser LispVal
parseVector = do
  lexeme $ string "#("
  vals <- parseExpr `sepEndBy` whiteSpace
  lexeme $ char ')'
  return . LVector $ vector vals


------- Helper Parsers -------

binChars :: String
binChars = "01"

-- http://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
readBin :: String -> [(Integer, String)]
readBin = readInt 2 (`elem` binChars) digitToInt

symbolChars :: String
symbolChars = "!$%&|*+/:<=>?@^_~-."

symbol :: Parser Char
symbol = oneOf symbolChars

spaces1 :: Parser ()
spaces1 =  skipMany1 space

notSpace :: Parser Char
notSpace = satisfy (/=' ')

sepByOnce :: Parser a -> Parser sep -> Parser (a, a)
sepByOnce p sep = (,) <$> p <* sep <*> p


------- Language Definition -------

lispDef :: LanguageDef ()
lispDef = emptyDef {
  T.commentStart    = "#|"
, T.commentEnd      = "|#"
, T.commentLine     = ";"
, T.nestedComments  = True
, T.identStart      = letter <|> symbol
, T.identLetter     = letter <|> symbol <|> digit
, T.caseSensitive   = True
}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser lispDef

identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

dot :: Parser String
dot = T.dot lexer

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer
