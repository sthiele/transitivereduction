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
import qualified Data.Vector.Unboxed as U (Vector,fromList)

import Statistics.Test.KolmogorovSmirnov
import Statistics.Sample

-- import Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D


                          

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
               Right m   -> let
                                all_pos   = get_pos_target_nodes edges  -- all targets nodes of positive edges
                                all_neg   = get_neg_target_nodes edges  -- all targets nodes of negative edges

                                graph = toGraph edges
                                tf_pos   = map (get_pos graph) t_factors        -- all positive successors of the tfs
                                tf_neg   = map (get_neg graph) t_factors        -- all negative successors of the tfs
                                acc = nub [(prop,0) | (node,prop) <- m]

                                all_phist = sort (check_properties m acc all_pos)
                                all_nhist = sort (check_properties m acc all_neg)      
                                tf_phist  = map sort (map (check_properties m acc) tf_pos)
                                tf_nhist  = map sort (map (check_properties m acc) tf_neg) 
                                
                                pstats = computestats all_phist tf_phist
                                nstats = computestats all_nhist tf_nhist
                            in
			    sequence_ $   
			    GP.plotDefault list2d :
			    []
-- 			    plotList [] (get_hist all_phist)
--                             putStrLn (
-- --                             (show acc)                            
--                             (show_hist all_phist)
--                             ++"\n\n"++
--                             (show_hist all_nhist)
--                             ++"\n\n"++
--                             (show_hists tf_phist)
--                             ++"\n\n"++
--                             (show_hists tf_nhist)
-- 			    ++"\n\n"++
--                             (show pstats)
-- 			    ++"\n\n"++                            
--                             (show nstats)                      
--                             )


list2d :: Plot2D.T Int Integer
list2d =
   Plot2D.list Graph2D.listPoints [0,1,1,2,3,5,8,13]
   
-- simple2d :: Plot2D.T Double Double
-- simple2d =
--    Plot2D.function Graph2D.lines
--       (linearScale 100 (-10,10)) sin
                            
get_hist [] = []
get_hist ((p,n):xs) = [n] ++ (get_hist xs)
                            
show_hists [] = ""
show_hists (x:xs) = (show_hist x) ++ "\n" ++ (show_hists xs)

show_hist [] = ""
show_hist ((p,n):xs) = (show n) ++ " " ++ (show_hist xs)
     
show_g:: [Edge] -> [Char]
show_g [] = ""
show_g (x:xs) = (show x)++"\n"++(show_g xs)
     

strip:: [(String,Int)] -> [Double]
strip [] = []
strip ((p,d):xs) = ((fromIntegral d):(strip xs))


-- type Sample = Vector Double

computestats:: [(String,Int)] -> [[(String,Int)]] -> [Double]
computestats ps [] = []
computestats ps1 (ps2:xs) = let s1 = U.fromList (strip ps1)
				s2 = U.fromList (strip ps2)
				res = kolmogorovSmirnov2D  s1 s2
		            in
			      (res:(computestats ps1 xs))

     
     