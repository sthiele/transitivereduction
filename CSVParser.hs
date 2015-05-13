-- Copyright (c) 2015, Sven Thiele <sthiele78@gmail.com>

-- This file is part of xxxx.

-- xxxx is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- xxxx is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with xxxx.  If not, see <http://www.gnu.org/licenses/>.

module CSVParser (
    readCSV,
    readCSV2,
    readProps,
) where
import Graph
import Data.Attoparsec.Text
import Data.Attoparsec.ByteString.Char8(isSpace)
import Control.Applicative ((<|>))
import Data.Text (pack)
import Data.Scientific as Scientific


csvParser:: Parser [Edge]
csvParser = do
  l <- (many' edgeParser)
  return l
  
  
edgeParser:: Parser Edge
edgeParser = do
  skipSpace  
  n1 <- nodeParser
  skipSpace
  sign <- signParser
  skipSpace
  n2 <- nodeParser
  skipSpace
  w <- Data.Attoparsec.Text.scientific
  return (Edge n1 n2 sign (toRealFloat w))
  


nodeParser:: Parser [Char]
nodeParser = do
  n <- identifier
  return n
  
signParser:: Parser Bool
signParser = 
  do
    string (Data.Text.pack "1")
    return True
  <|>
  do 
    string (Data.Text.pack "-1")
    return False  
  
identifier:: Parser String
identifier = do
  s <- (many1 mchar)
  return s

mchar:: Parser Char
mchar =
  do
    l <- letter
    return l
  <|>
  do
    d <- digit
    return d  
  <|>
  do
    c <- char '_'
    return c
  <|>
  do
    c <- char '-'
    return c
  <|>
  do
    c <- char '.'
    return c

    
identifierr:: Parser String
identifierr = do
  l <-letter
  s <- (many1 mchar)
  return (l:s)

-----------                   
readCSV input =  parseOnly csvParser (Data.Text.pack input)



skipSpaceNoNewline = skipWhile (\x -> isSpace x && not (isEndOfLine x))

type MyMAT = ([String],[String],[[Double]])

csvParser2:: Parser MyMAT
csvParser2 =
--   let (rowids,dta) = unscramble rows in
  do
  columnids <- headerParser
  endOfLine
  rows <- (many' rowParser)
  let (rowids,dta) = unscramble rows in
      return (columnids, rowids, dta)

headerParser:: Parser [String]
headerParser =
  do
  skipSpaceNoNewline
  columnids <- (many' colidParser)
  return columnids
  
type MyROW = (String,[Double])

colidParser:: Parser String
colidParser = do
  colid <- identifierr
  skipSpaceNoNewline 
  return colid


rowParser:: Parser MyROW
rowParser = do
  skipSpaceNoNewline
  rowid <- identifierr
  skipSpaceNoNewline
  dta <- (many' cellParser)
  endOfLine
  return (rowid, dta)


cellParser:: Parser Double
cellParser = do
  cell <- Data.Attoparsec.Text.scientific
  skipSpaceNoNewline
  return (toRealFloat cell)
  

unscramble:: [MyROW] -> ([String],[[Double]])
unscramble [] = ([],[])
unscramble ((id,dta):rest) =
  let (ids,dtas) = unscramble rest in
      ((id:ids),(dta:dtas))
      
readCSV2 input = parseOnly csvParser2 (Data.Text.pack input)

readProps input = parseOnly propsParser (Data.Text.pack input)

propsParser:: Parser [(String,String)]
propsParser =
  do
  props <- (many' propParser)
  return props

propParser:: Parser (String,String)
propParser = do
  skipSpaceNoNewline
  node <- identifierr
  skipSpaceNoNewline
  prop <- propertyParser
  endOfLine
  return (node, prop)

anyNoNewline = satisfy (not . isEndOfLine)


propertyParser:: Parser String
propertyParser = do
  property <- (many' anyNoNewline)
  return property
