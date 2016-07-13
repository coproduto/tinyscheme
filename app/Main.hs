module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispString String
             | LispBool Bool
             | LispCharacter Char
             | LispFloat Float
               deriving Show

data NumberBase = Binary
                | Octal
                | Decimal
                | Hexadecimal

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = 
    case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ show err
      Right val -> "Found value: " ++ show val


{-------------------------------------------------------------------------------
--------------------------------------------------------------------------------

DATATYPE PARSERS

--------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

parseExpr :: Parser LispVal
parseExpr = 
    try parseChar
            <|> (try parseNumber)
            <|> parseAtom
            <|> parseString

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (nonEscape <|> escapeSequence)
  char '"'
  return $ LispString x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbolChar
  rest <- many (letter <|> digit <|> symbolChar)
  let atom = first:rest
  return $ case atom of
             "#t" -> LispBool True
             "#f" -> LispBool False
             _    -> LispAtom atom

--TODO: Implement negative numbers
parseNumber :: Parser LispVal
parseNumber = do 
  prefix <- option Decimal numberPrefix
  case prefix of
    Binary      -> parseBinary
    Octal       -> parseOctal
    Decimal     -> parseDecimal
    Hexadecimal -> parseHexadecimal

--TODO: Implement named chars
parseChar :: Parser LispVal
parseChar = do
  char '#'
  char '\\'
  c <- anyChar
  return $ LispCharacter c

parseBinary :: Parser LispVal
parseBinary = do
  digits <- many1 binDigit
  return $ (LispNumber . fst . head . readBin) digits
      where readBin = readInt 2 (flip elem binDigits) (\c -> ord c - ord '0')

parseOctal :: Parser LispVal
parseOctal = do
  digits <- many1 octDigit
  return $ (LispNumber . fst . head . readOct) digits

parseDecimal :: Parser LispVal
parseDecimal = do
  digits <- many1 digit
  return $ (LispNumber . read) digits

parseHexadecimal = do
  digits <- many1 hexDigit
  return $ (LispNumber . fst . head . readHex) digits

--TODO: Finish this
{-
parseFloat :: Parser LispVal
parseFloat = do
  integerPart <- parseDecimal
  char '.'
  fractionPart <- parseDecimal
-}  

{-------------------------------------------------------------------------------
--------------------------------------------------------------------------------

HELPER PARSERS

--------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

numberPrefix :: Parser NumberBase
numberPrefix = do
  char '#'
  prefix <- oneOf numberPrefixes
  return $ case prefix of
             'b' -> Binary
             'o' -> Octal
             'd' -> Decimal
             'x' -> Hexadecimal


nonEscape :: Parser Char
nonEscape = noneOf "\\\""

escapeSequence :: Parser Char
escapeSequence = do
  char '\\'
  escaped <- oneOf escapeChars
  return $ case escaped of
             '"'  -> '"'
             'n'  -> '\n'
             't'  -> '\t'
             'r'  -> '\r'
             '\\' -> '\\'

binDigit :: Parser Char
binDigit = oneOf binDigits

symbolChar :: Parser Char
symbolChar = 
    oneOf symbolChars

spaces :: Parser ()
spaces = 
    skipMany1 space

{-------------------------------------------------------------------------------
--------------------------------------------------------------------------------

DECLARATIONS

--------------------------------------------------------------------------------
-------------------------------------------------------------------------------}

symbolChars :: [Char]
symbolChars = "!#$%&|*+-/:<=>?@^_~"

escapeChars :: [Char]
escapeChars = "nrt\\\""

numberPrefixes :: [Char]
numberPrefixes = "bodx"

binDigits :: [Char]
binDigits = "01"

floatSuffixes :: [Char]
floatSuffixes = "sfdl"
