{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Debug.Trace
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B


data Value = Value String deriving (Show, Eq)

data Argument = Argument String String deriving (Show, Eq)

data Cell = Cell String String [Value] deriving (Show, Eq)

data Column = Column String String [Argument] [Cell] deriving (Show, Eq)

data Spreadsheet = Spreadsheet Version [Column] deriving (Show, Eq)

data Version = Version String deriving (Show, Eq)

argParser :: Parser Argument
argParser = do
    name <- many $ satisfy (`notElem` "\n|")
    char '|'
    colType <- many $ satisfy (`notElem` ",\n")
    return $ Argument name colType

charParser :: Parser Char
charParser = do
    satisfy (`notElem` ",)\\\n") <|> char '\\' *> anyChar

valueParser :: Parser Value
valueParser = do
    value <- many $ charParser
    return $ Value value

cellValueParser :: Parser [Value]
cellValueParser = do
    char '('
    values <- sepBy1 valueParser $ char ','
    char ')'
    return values

cellParser :: Parser Cell
cellParser = do
    onset <- many $ notChar ','
    char ','
    offset <- many $ notChar ','
    char ','
    values <- cellValueParser
    return $ Cell onset offset values

versionParser :: Parser Version
versionParser = do
    version <- many $ notChar '\n'
    char '\n'
    return $ Version version

columnParser :: Parser Column
columnParser = do
    name <- many $ notChar ' '
    trace(name) char ' '
    char '('
    colType <- many $ notChar ','
    trace(colType) char ','
    many $ notChar ')'
    char ')'
    char '-'
    args <- many $ argParser
    trace(show args) char '\n'
    cells <- many $ cellParser <* endOfLine
    return $ Column name colType args cells

fileParser :: Parser Spreadsheet
fileParser = do
    version <- versionParser
    columns <- many $ columnParser
    return $ Spreadsheet version columns

----------------------
---------- MAIN --------
------------------------
main :: IO ()
main = do
    file <- T.readFile "/Users/jesse/Desktop/CrawlingObstacles-102-1/testing"
    print "Parsing"
    print $ parseOnly fileParser file
