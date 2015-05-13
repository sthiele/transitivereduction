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
import Data.List (nub, sort)

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
                          

t_factors =["trpI","gpuR","mexR","vfr","pchR","ampR","rocA1","rocR","tpbA","nalC","PA3697","PA14_18080","rhlR","hasR","flgM"
            ,"ppyR","gacA","mexT","bexR","fpvR","sfnR","ptxR","qscR","cysB","exsD","anr","lasR","PA1301","phoQ","fleQ","mvfR"
            ,"gacS","argR","phhR","PA0779","mucD","pprB","mvaT","algW","roxS","rcsB","pilR","nfxB","cbrB","pmrA","algQ","algR"
            ,"algZ","PA14_70560","PA5471"]
                       
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
         content2 <- readFile file2 -- read mapping properties
         case readCSV content1 of
           Left  err   -> putStrLn ("ParseError: " ++ show err)
           Right edges ->
             case readProps content2 of
               Left  err -> putStrLn ("ParseError: " ++ show err)
               Right m   -> let graph = toGraph edges
--                                 ds    = map (get_ds graph) t_factors
                                pos   = map (get_pos graph) t_factors
                                neg   = map (get_neg graph) t_factors
                                acc = nub [(prop,0) | (node,prop) <- m]
--                                 hist  = map (check_properties m acc) ds
--                                 shist = map sort hist
                                phist = map sort (map (check_properties m acc) pos)
                                nhist = map sort (map (check_properties m acc) neg)              
                            in
                            putStrLn (
                             (show pos)
                              ++"\n\n"++
                             (show neg)
--                             (show_hists phist)
--                             ++"\n\n"++
--                             (show_hists nhist)
                            )
show_hists [] = ""
show_hists (x:xs) = (show_hist x) ++ "\n" ++ (show_hists xs)

show_hist [] = ""
show_hist ((p,n):xs) = (show p) ++ " " ++ (show_hist xs)
     
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
     
     