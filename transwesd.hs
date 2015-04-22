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

import System.Environment
import SIFParser
import CSVParser
import Graph

     
main:: IO ()  
main =
  do
    args <- getArgs
    if args==[]
       then putStrLn "No arguments given!"
       else do
         putStrLn ("TransWesd in Haskell.")
         contents <- readFile (head args)
         case readCSV contents of
           Left  err -> putStrLn ("ParseError: " ++ show err)
           Right val -> putStrLn (show_g val)
   
     
     
show_g:: [Edge] -> [Char]
show_g [] = ""
show_g (x:xs) = (show x)++"\n"++(show_g xs)
     
sif1 = "a + b\n"
    ++ "a + c\n"
    ++ "a + d\n"
    ++ "b + c\n"
    ++ "c - b\n"
    ++ "d + b\n"
    ++ "d - e\n"
    ++ "e + c\n"
Right g1 = readSIF sif1
     
     