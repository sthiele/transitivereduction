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
import System.Random
import Debug.Trace

myfilter:: Edge -> Bool
myfilter (Edge s t sign w) =
  let log2 = abs (logBase 2 w) in
  if log2 < 1
     then
--      trace ("weight "++(show w)++" abslog2 "++(show log2)++"\n") $
     True
     else
--      trace ("weight "++(show w)++" abslog2 "++(show log2)++"\n") $
     False
    
mytransformation:: Edge -> Edge
mytransformation (Edge s t sign w) =
  let new_w = 20 * (abs w) in
  (Edge s t sign new_w)

-- normalize :: (Floating a) => [a] -> [a]
-- normalize vec = map (/ (sqrt $ sum (map (^2) vec))) vec

normalize vec =
  let m = minimum vec
      s = (maximum vec)-m
  in
    map (norm m s) vec

norm mu sigma x = (x -mu)/sigma
                          
     
main:: IO ()  
main =
  do
    args <- getArgs
    if length args<2
       then putStrLn "to few arguments given!"
       else
         let (file1:file2:_)=args in
         do
         putStrLn ("\n-- Placeholder ----------------------------------------\n")
         content1 <- readFile file1
         content2 <- readFile file2
         case readCSV2 content1 of
           Left  err -> putStrLn ("ParseError: " ++ show err)
           Right (s,t,adjmat) ->
             case readCSV2 content2 of
                  Left err -> putStrLn ("ParseError: " ++ show err)
                  Right (_,_,logmat) -> let e = createEdgesfromMat (s,t,adjmat,logmat)
                                            e2 = transformEdges e mytransformation
                                            graph = toGraph e2
                                        in
                                          
                                          putStrLn (
                                            (show (transwesd graph 0.95))
                                          )
                              




--                         let graph = val
--                             a = apply_to_weights graph normalize
--                             b = filterEdges a myfilter
--                             c = transformEdges b mytransformation
--                             g = toGraph a
-- --                             (c1,c2,c3,c4,i1,i2,i3,i4) = get_motifs g
--                             c1 = get_motif1 g
--                             c2 = get_motif2 g
--                             c3 = get_motif3 g
--                             c4 = get_motif4 g
--                             i1 = get_motif5 g
--                             i2 = get_motif6 g
--                             i3 = get_motif7 g
--                             i4 = get_motif8 g
-- 
--                             gen = mkStdGen 100
--                             (rgraph, ngen) = createRGraph gen 100
--                         in
--                         trace ("ori "++(show (length a))++" filtered "++(show (length b))++"\n") $
--                         putStrLn (
-- --                             "graph " ++ (show graph) ++"\n" ++
-- --                             "normalized " ++ (show a) ++"\n" ++
-- --                             "filtered " ++ (show b) ++"\n" ++
--                             
--                             "in nodes:       " ++ (show (get_in_nodes g))++"\n"
--                          ++ "out nodes:      " ++ (show (get_out_nodes g))++"\n"
--                          ++ "\n-- Cycles ---------------------------------------------\n"
--                          ++ "cyclic nodes:   " ++ (show (get_cyclic_nodes g))++"\n"
--                          ++ "negcyclic node: " ++ (show (get_neg_cyclic_nodes g))++"\n"
--                          ++ "\n-- Feedforward loops ----------------------------------\n"
--                          ++ "C1-FFl: " ++ (show c1)++"\n"
--                          ++ "C2-FFl: " ++ (show c2)++"\n"
--                          ++ "C3-FFl: " ++ (show c3)++"\n"
--                          ++ "C4-FFl: " ++ (show c4)++"\n"
--                          ++ "I1-FFl: " ++ (show i1)++"\n"
--                          ++ "I2-FFl: " ++ (show i2)++"\n"
--                          ++ "I3-FFl: " ++ (show i3)++"\n"
--                          ++ "I4-FFl: " ++ (show i4)++"\n"
--                          ++ "\n-- Dependency matrix ----------------------------------\n"
--                          ++ (matrix2string (dep_matrix g))++"\n"
--                          ++ " transwesd: " ++(show (transwesd g (0.95)))++"\n"
--                          
--                         )


     
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
     
     