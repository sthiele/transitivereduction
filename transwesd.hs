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
         putStrLn ("\n-- Placeholder ----------------------------------------\n")
         contents <- readFile (head args)
         case readCSV contents of
           Left  err -> putStrLn ("ParseError: " ++ show err)
           Right val -> let g = toGraph val in                      
--                         putStrLn (show (transwesd g (0.95)))
                        putStrLn (
                            "in nodes:       " ++ (show (get_in_nodes g))++"\n"
                         ++ "out nodes:      " ++ (show (get_out_nodes g))++"\n"
                         ++ "\n-- Cycles ---------------------------------------------\n"
                         ++ "cyclic nodes:   " ++ (show (get_cyclic_nodes g))++"\n"
                         ++ "negcyclic node: " ++ (show (get_neg_cyclic_nodes g))++"\n"
                         ++ "\n-- Feedforward loops ----------------------------------\n"
                         ++ "C1-FFl: " ++ (show (get_motif1 g))++"\n"
                         ++ "C2-FFl: " ++ (show (get_motif2 g))++"\n"
                         ++ "C3-FFl: " ++ (show (get_motif3 g))++"\n"
                         ++ "C4-FFl: " ++ (show (get_motif4 g))++"\n"
                         ++ "I1-FFl: " ++ (show (get_motif5 g))++"\n"
                         ++ "I2-FFl: " ++ (show (get_motif6 g))++"\n"
                         ++ "I3-FFl: " ++ (show (get_motif7 g))++"\n"
                         ++ "I4-FFl: " ++ (show (get_motif8 g))++"\n"
                         ++ "\n-- Dependency matrix ----------------------------------\n"
                         ++ (matrix2string (dep_matrix g))++"\n"
                        )


     
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
     
     