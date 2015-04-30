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

module Graph (
  Node(..),
  Graph(..),
  toGraph,
  Edge(..),
  transwesd,
  get_in_nodes,
  get_out_nodes,
  get_cyclic_nodes,
  get_neg_cyclic_nodes,    
  get_motif1,
  get_motif2,
  get_motif3,     
  get_motif4,
  get_motif5,
  get_motif6,
  get_motif7,
  get_motif8,
  dep_matrix,
  matrix2string,
)
where

import Data.Maybe
import Data.List (nub, (\\), delete, intersperse )
import qualified Data.Map as Map
import Debug.Trace


data Node = Node { name::String
                 , posdep::[(String,Double)]
                 , negdep::[(String,Double)]
}

instance Show Node where
  show (Node n pdep ndep) = n
instance Eq Node where
  (Node n1 pdep1 ndep1) == (Node n2 pdep2 ndep2) = n1==n2



data Edge = Edge { source::String
                 , target::String
                 , sign::Bool
                 , weight::Double
}
instance Show Edge where
  show (Edge s t sign w) = s++" -> "++t++" "++(show sign)++" "++(show w)



type Graph =  Map.Map (String) (Node)

toGraph:: [Edge] -> Graph
toGraph x = toGraph2 x Map.empty

toGraph2 [] g = g
toGraph2 (e:es) g =
  let g' = addEdge g e in toGraph2 es g'

addEdge:: Graph -> Edge -> Graph
addEdge g (Edge source target sign weight) =
  let g1 = addNode g source
      g2 = addNode g1 target
  in  
  case Map.lookup source g2 of
  Just (Node name pdep ndep) -> if sign
                                then Map.insert source (Node source ((target,weight):pdep) ndep) g2
                                else Map.insert source (Node source pdep ((target,weight):ndep)) g2
  
  Nothing -> if sign
             then Map.insert source (Node source [(target,weight)] []) g2
             else Map.insert source (Node source [] [(target,weight)]) g2


addNode:: Graph -> String -> Graph
addNode g name =
  case Map.lookup name g of
  Just x -> g
  Nothing -> Map.insert name (Node name [] []) g


             
getnodes:: [(String,Double)] -> [String]
getnodes [] = []
getnodes ((n,d):rest) = (n:(getnodes rest))


has_pathto:: Graph -> String -> [String] -> String -> Bool
has_pathto graph goal visited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let dep = (getnodes posdep) ++ (getnodes negdep) in
                                    if elem goal dep
                                    then True
                                    else
                                      let new_visited = (name:visited) in
                                      or (map (has_pathto graph goal new_visited) (dep\\new_visited))
                  
  _ -> False
                                                

has_pospathto:: Graph -> String -> [String] -> [String] -> String -> Bool
has_pospathto graph goal pvisited nvisited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = getnodes posdep
                                        ndep = getnodes negdep
                                    in
                                    if elem goal pdep
                                    then True
                                    else
                                      let new_pvisited = (name:pvisited)
                                      in
                                      or ((map (has_pospathto graph goal new_pvisited nvisited ) (pdep\\new_pvisited)) ++ (map (has_negpathto graph goal new_pvisited nvisited) (ndep\\nvisited)))
  _ -> False



  
has_negpathto:: Graph -> String -> [String] -> [String] -> String -> Bool
has_negpathto graph goal pvisited nvisited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = getnodes posdep
                                        ndep = getnodes negdep
                                    in
                                    if elem goal ndep
                                    then True
                                    else
                                      let new_nvisited = (name:nvisited)
                                      in
                                      or ((map (has_negpathto graph goal pvisited new_nvisited) (pdep\\pvisited)) ++ (map (has_pospathto graph goal pvisited new_nvisited) (ndep\\new_nvisited)))
  _ -> False
 
has_cyclefreepospathto:: Graph -> String -> [String] -> String -> Bool
has_cyclefreepospathto graph goal visited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = getnodes posdep
                                        ndep = getnodes negdep
                                    in
                                    if elem goal pdep
                                    then True
                                    else
                                      let new_visited = (name:visited)
                                      in
                                      or ((map (has_cyclefreepospathto graph goal new_visited) (pdep\\new_visited)) ++ (map (has_cyclefreenegpathto graph goal new_visited) (ndep\\new_visited)))
  _ -> False

has_cyclefreenegpathto:: Graph -> String -> [String] -> String -> Bool
has_cyclefreenegpathto graph goal visited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = getnodes posdep
                                        ndep = getnodes negdep
                                    in
                                    if elem goal ndep
                                    then True
                                    else
                                      let new_visited = (name:visited)
                                      in
                                      or ((map (has_cyclefreenegpathto graph goal new_visited) (pdep\\new_visited)) ++ (map (has_cyclefreepospathto graph goal new_visited) (ndep\\new_visited)))
  _ -> False

  
hasCycle:: Graph -> Bool
hasCycle g =
  let nodes = Map.keys g in
  if nodes == []
     then True
     else or  (map (hasCycleN g) nodes)

hasCycleN g n = has_pathto g n [] n


hasnegCycle:: Graph -> Bool
hasnegCycle g =
  let nodes = Map.keys g in
  if nodes == []
     then True
     else or  (map (hasnegCycleN g) nodes)

hasnegCycleN g n = has_negpathto g n [] [] n


shortespathto:: Graph -> String -> [String] -> String -> Maybe Double
shortespathto graph goal visited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) ->  let dep = (getnodes posdep) ++ (getnodes negdep) in
                                     if elem goal dep
                                     then Just 1
                                     else
                                       let new_visited = (name:visited)
                                           mini = (maybe_minimum (map (shortespathto graph goal new_visited) (dep\\new_visited)))
                                       in
                                       case mini of
                                            Nothing -> Nothing
                                            Just x  -> Just (1+x)

  _ -> Nothing


shortepospathto graph goal pvisited nvisited max (cur,w) =
    if w > max
     then shortespospathto graph goal pvisited nvisited w cur
     else shortespospathto graph goal pvisited nvisited max cur
       
shortespospathto:: Graph -> String -> [String] -> [String] -> Double -> String -> Maybe Double
shortespospathto graph goal pvisited nvisited max cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    case ffind goal posdep of
                                    Just x -> if x > max then Just x
                                                         else Just max
                                    _ ->
                                      let new_pvisited = (name:pvisited)
                                          mini = (maybe_minimum ( (map (shortepospathto graph goal new_pvisited nvisited max) (wo posdep new_pvisited))
                                                                ++(map (shortenegpathto graph goal new_pvisited nvisited max) (wo negdep nvisited)))
                                                 )
                                      in
                                      case mini of
                                      Nothing -> Nothing
                                      Just x  -> if x > max then Just x
                                                            else Just max
                                                  
  _ -> Nothing

ffind:: String -> [(String,Double)] -> Maybe Double
ffind x [] = Nothing
ffind x ((s,w):rest) =
  if s==x
     then Just w
     else ffind x rest

wo::  [(String,Double)] -> [String] -> [(String,Double)]
wo [] _ = []
wo ((n,w):rest) names = if elem n names
                           then wo rest names
                           else ((n,w):(wo rest names))

shortenegpathto graph goal pvisited nvisited max (cur,w) =
  if w > max
     then shortesnegpathto graph goal pvisited nvisited w cur
     else shortesnegpathto graph goal pvisited nvisited max cur
     
shortesnegpathto:: Graph -> String -> [String] -> [String] -> Double -> String -> Maybe Double
shortesnegpathto graph goal pvisited nvisited max cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    case ffind goal negdep of
                                    Just x -> if x > max then Just x
                                                         else Just max
                                    _ ->
                                      let new_nvisited = (name:nvisited)
                                          mini = (maybe_minimum ( (map (shortenegpathto graph goal pvisited new_nvisited max) (wo posdep pvisited))
                                                                ++(map (shortepospathto graph goal pvisited new_nvisited max) (wo negdep new_nvisited)))
                                                 )
                                      in
                                      case mini of
                                      Nothing -> Nothing
                                      Just x  -> if x > max then Just x
                                                            else Just max

  _ -> Nothing  
     
maybe_minimum:: [Maybe Double] -> Maybe Double
maybe_minimum [] = Nothing
maybe_minimum (Nothing:rest) = 
  let r = (maybe_minimum rest) in
  case r of
    Nothing -> Nothing
    Just y  -> Just y

maybe_minimum ((Just x):rest) = 
  let r = (maybe_minimum rest) in
  case r of
    Nothing -> Just x
    Just y  -> case compare x y of
		    LT -> Just x
		    _  -> Just y



getCandidateEdges:: Graph -> Double -> [Maybe Edge]
getCandidateEdges graph alpha =
  let nodes = Map.keys graph
      tmp =  map (check graph alpha) nodes
  in
  flatten tmp

flatten:: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x++(flatten xs)


check:: Graph -> Double -> String -> [Maybe Edge]
check graph alpha nodei =
  case Map.lookup nodei graph of 
  Just (Node name posdep negdep) ->
    let e1 = map (check_pos graph alpha nodei) posdep
        e2 = map (check_neg graph alpha nodei) negdep
    in e1++e2
  Nothing -> []


  
check_pos:: Graph -> Double ->  String -> (String,Double) -> Maybe Edge
check_pos graph alpha nodei (nodek,weight) =
--   trace ("checkp "++(show nodei)++(show nodek)++"\n"
--   ) $
  case Map.lookup nodei graph of
       Just (Node name posdep negdep) -> let pdep = delete (nodek,weight) posdep
                                             ndep = negdep
                                             weights = (map second pdep) ++ (map second ndep)
                                             spaths = (map (shortepospathto graph nodek [] [] 0) pdep) ++ (map (shortenegpathto graph nodek [] [] 0) ndep)
                                             bothscores = zip weights spaths
                                         in
--                                          trace ( " show scores" ++"\n"
--                                          ++"paths"++ (show paths) ++"\n"
--                                          ++"spaths"++ (show spaths) ++"\n"
--                                          ) $
                                         if (test (alpha*weight) bothscores)
                                         then Just (Edge nodei nodek True weight)
                                         else Nothing
       _ -> Nothing

check_neg:: Graph -> Double ->  String -> (String,Double) -> Maybe Edge
check_neg graph alpha nodei (nodek,weight) =
--   trace ("checkn "++(show nodei)++" "++(show nodek)++"\n"
--   ) $  
  case Map.lookup nodei graph of
       Just (Node name posdep negdep) -> let pdep = posdep
                                             ndep = delete (nodek,weight) negdep
                                             weights = (map (second) pdep) ++ (map (second) ndep)
                                             spaths = (map (shortenegpathto graph nodek [] [] 0) pdep) ++ (map (shortepospathto graph nodek [] [] 0) ndep)
                                             bothscores = zip weights spaths
                                         in
--                                          trace ( " show scores" ++"\n"
--                                          ++"paths"++ (show paths) ++"\n"
--                                          ++"spaths"++ (show spaths) ++"\n"
--                                          ) $
                                         if (test (alpha*weight) bothscores)
                                         then Just (Edge nodei nodek False weight)
                                         else Nothing                                         
       _ -> Nothing
                                         
second (node,weight) =  Just weight


test:: Double -> [(Maybe Double,Maybe Double)] -> Bool
test k [] = False
test k ((Just a,Just b):xs) =
  trace ( "test k="++ (show k)++"vs\n"
  ++"a="++(show a)++" b="++(show b)++"\n"
  ) $
  if ((a < k) && (b < k))
     then True
     else test k xs
test k ((_,_):xs) = test k xs
     
     
transwesd:: Graph -> Double -> [Maybe Edge]
transwesd graph alpha =
  let check = hasnegCycle graph
      nodes = Map.keys graph
      candidates = getCandidateEdges graph alpha
  in
  candidates
     

check_cycle:: Graph -> String -> Maybe String
check_cycle graph n =
  if (has_pathto graph n [] n)
     then Just n
     else Nothing

     
get_cyclic_nodes:: Graph -> [String]
get_cyclic_nodes graph =
  let nodes = Map.keys graph
      maybe_cnodes = map (check_cycle graph) nodes
  in
  catMaybes maybe_cnodes


check_neg_cycle:: Graph -> String -> Maybe String
check_neg_cycle graph n =
  if (has_negpathto graph n [] [] n)
     then Just n
     else Nothing


get_neg_cyclic_nodes:: Graph -> [String]
get_neg_cyclic_nodes graph =
  let nodes = Map.keys graph
      maybe_cnodes = map (check_neg_cycle graph) nodes
  in
  catMaybes maybe_cnodes
  
     
get_motif1:: Graph -> [(String,String,String)]
get_motif1 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt1 graph) combies
  in
  catMaybes maybe_motifs
  
checkmt1:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt1 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes pdx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes pdy))
                    then Just (x,y,z)
                    else Nothing
  
     
get_motif2:: Graph -> [(String,String,String)]
get_motif2 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt2 graph) combies
  in
  catMaybes maybe_motifs

checkmt2:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt2 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes ndx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes pdy))
                    then Just (x,y,z)
                    else Nothing     
     
get_motif3:: Graph -> [(String,String,String)]
get_motif3 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt3 graph) combies
  in
  catMaybes maybe_motifs

checkmt3:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt3 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes ndx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes ndy))
                    then Just (x,y,z)
                    else Nothing  


get_motif4:: Graph -> [(String,String,String)]
get_motif4 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt4 graph) combies
  in
  catMaybes maybe_motifs

checkmt4:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt4 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes pdx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes ndy))
                    then Just (x,y,z)
                    else Nothing

get_motif5:: Graph -> [(String,String,String)]
get_motif5 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt5 graph) combies
  in
  catMaybes maybe_motifs

checkmt5:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt5 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes pdx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes ndy))
                    then Just (x,y,z)
                    else Nothing

get_motif6:: Graph -> [(String,String,String)]
get_motif6 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt6 graph) combies
  in
  catMaybes maybe_motifs

checkmt6:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt6 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes ndx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes ndy))
                    then Just (x,y,z)
                    else Nothing

get_motif7:: Graph -> [(String,String,String)]
get_motif7 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt7 graph) combies
  in
  catMaybes maybe_motifs

checkmt7:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt7 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes ndx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes pdy))
                    then Just (x,y,z)
                    else Nothing

get_motif8:: Graph -> [(String,String,String)]
get_motif8 graph =
  let nodes = Map.keys graph
      combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes]
      maybe_motifs = map (checkmt8 graph) combies
  in
  catMaybes maybe_motifs

checkmt8:: Graph -> (String,String,String) -> Maybe (String,String,String)
checkmt8 graph (x,y,z) =
  case Map.lookup x graph of
       Nothing -> Nothing
       Just (Node nx pdx ndx) ->
        case Map.lookup y graph of
            Nothing -> Nothing
            Just (Node ny pdy ndy) ->
             case Map.lookup z graph of
                 Nothing -> Nothing
                 Just (Node nz pdz ndz) ->
                  if (elem nz (getnodes pdx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes pdy))
                    then Just (x,y,z)
                    else Nothing


get_in_nodes:: Graph -> [String]
get_in_nodes graph =
  let names = Map.keys graph
      nodes = Map.elems graph
      pdeps = [ getnodes pd | (Node  n pd nd) <- nodes ]
      ndeps = [ getnodes nd | (Node  n pd nd) <- nodes ]
      has_in = nub ((flatten pdeps) ++  (flatten ndeps))
      in_nodes = names \\ has_in
  in
  in_nodes
  
get_out_nodes:: Graph -> [String]
get_out_nodes graph =
  let nodes = Map.elems graph
--       out_nodes1 = [ n | (Node  n pd nd) <- nodes, nd==[] ]
      out_nodes = [ n | (Node  n [] []) <- nodes ]
  in
  out_nodes


data Infl = Pos | Neg | Amb | No

instance Show Infl where
  show Pos = "+"
  show Neg = "-"
  show Amb = "~"
  show No  = " "
  
dep_matrix:: Graph -> [(String,String,Infl)]
dep_matrix graph =
  let inputs = get_in_nodes graph
      outputs = get_out_nodes graph
      matrix = [ (i,o) | i <- inputs, o <- outputs]
  in
  map (get_influence graph) matrix

get_influence:: Graph -> (String,String) -> (String,String,Infl)
get_influence graph (i,o) =
  if has_cyclefreepospathto graph o [] i
  then
     if has_cyclefreenegpathto graph o [] i
     then (i,o,Amb)
     else (i,o,Pos)
  else
     if has_cyclefreenegpathto graph o [] i
     then (i,o,Neg)
     else (i,o,No)

matrix2string:: [(String,String,Infl)] -> String
matrix2string [] = ""
matrix2string m =
  let ins =  nub [ i | (i,o,v) <-m]
      outs = nub [ o | (i,o,v) <-m]
      table = map (get_readouts m) outs
  in
  " \t|"++ (flatten (intersperse " | " ins)) ++"\n"++ (flatten (intersperse "\n" table))
      

get_readouts:: [(String,String,Infl)] ->  String -> String
get_readouts m r =
  let rx = [ (show v) | (x,o,v) <- m, r==o]
      bla = r++ " \t| "++ (flatten (intersperse " | " rx))
  in bla