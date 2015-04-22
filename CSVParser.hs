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
    readCSV
) where
import Graph
import Data.Attoparsec.Text
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


-----------                   
readCSV input =  parseOnly csvParser (Data.Text.pack input)






