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
  createEdgesfromMat,
  Node(..),
  Edge(..),
  toGraph,
  filterEdges,
  transformEdges,
  getweights,
  apply_to_weights,
  Graph(..),
  createRGraph,
  transwesd,
  get_in_nodes,
  get_out_nodes,
  get_cyclic_nodes,
  get_neg_cyclic_nodes,    
  get_motifs,
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
  get_ds,
  get_pos,
  get_neg,
  check_properties,
)
where

import Data.Maybe
import Data.List (nub, (\\), delete, intersperse )
import qualified Data.Map as Map
import System.Random
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

createEdgesfromMat :: ([String],[String],[[Double]],[[Double]]) -> [Edge]

createEdgesfromMat ([],_,_,_) = []
createEdgesfromMat ((s:rs),targets, adj, log2s) =
  let target_adj = map head adj
      target_log2 = map head log2s
      remain_adj = map tail adj 
      remain_logs = map tail log2s
      edges1 = createEdgesfromTargetlist (s,targets,target_adj,target_log2)
  in
  edges1++( createEdgesfromMat (rs,targets,remain_adj,remain_logs))

createEdgesfromTargetlist:: (String    -- source node
                            ,[String]  -- list of target nodes
                            ,[Double]  -- list of adj values
                            ,[Double]) -- list of log2 values
                         -> [Edge]     -- list of edges
                        
createEdgesfromTargetlist (s,[],_,_) = []
createEdgesfromTargetlist (s,(t:ts),(a:as),(l:ls)) =
  if a < 0.05
  then
    if l > 1
    then ((Edge s t True a):(createEdgesfromTargetlist (s,ts,as,ls)))
    else
      if l < -1
      then ((Edge s t False a):(createEdgesfromTargetlist (s,ts,as,ls)))
      else (createEdgesfromTargetlist (s,ts,as,ls))
  else (createEdgesfromTargetlist (s,ts,as,ls))

  

  
filterEdges:: [Edge] -> (Edge -> Bool) -> [Edge]
filterEdges [] _ = []
filterEdges (e:es) f = if f e
                          then (e:(filterEdges es f))
                          else (filterEdges es f)


getweights  [] = []
getweights  ((Edge s t sign w) :es) = (w:(getweights es))


setweights  [] _ = []
setweights  _ [] = []
setweights  ((Edge s t sign w) :es) (nw:ws)= ((Edge s t sign nw):(setweights es ws))

apply_to_weights:: [Edge] -> ([Double]->[Double]) -> [Edge]
apply_to_weights es f = let ws = getweights es
                            nws = f ws
                        in
                        setweights es nws

transformEdges:: [Edge] -> (Edge -> Edge) -> [Edge]
transformEdges [] _ = []
transformEdges (e:es) f = (f e:(transformEdges es f))
                            
type Graph =  Map.Map (String) (Node)

createRGraph:: StdGen -> Int -> ([Edge],StdGen)
createRGraph gen n =
    let nodes = [1..n]
        (nedges, ngen) = createRGraph2 gen nodes nodes
    in
    (nedges,ngen)

createRGraph2:: StdGen -> [Int] -> [Int] -> ([Edge],StdGen)
createRGraph2 gen [] _ =([],gen)
createRGraph2 gen (n:ns) targets =
  let stargets = targets \\ [n] -- no self loop
      (edges, ngen) = createREdges gen stargets n
      (rest,nngen) = createRGraph2 ngen ns targets
  in
  ((edges++rest),nngen)

    
createREdges:: StdGen -> [Int] -> Int -> ([Edge],StdGen)
createREdges gen [] node = ([],gen)
createREdges gen (n:ns) node =
  let
      (randomnumber1, gen2) = randomR (1,100) gen :: (Int, StdGen)
      (randomsign, gen3) = randomR (1,100) gen2 :: (Int, StdGen)
      (randomvalue, gen4) = randomR (0.0,1.0) gen3 :: (Double, StdGen)
      (rest,gen5) = (createREdges gen4 ns node)
  in
--   trace ("randomnumber " ++ (show randomnumber1)
--       ++ " randomsign " ++ (show (randomsign>10))
--       ++ " randomvalue " ++ (show randomvalue)
--        ) $
  if randomnumber1 < 30
  then
   ( ((Edge (show node) (show n) (randomsign>10) randomvalue):rest), gen5)
  else (rest,gen5)



  
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
                                      or ((map (has_negpathto graph goal pvisited new_nvisited) (pdep\\new_nvisited)) ++ (map (has_pospathto graph goal pvisited new_nvisited) (ndep\\pvisited)))
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
    trace ("try pos node "++cur++" weight "++(show w)++"\n") $
    if w > max
     then
--      trace ("update max "++(show w)++"\n") $
     shortespospathto graph goal pvisited nvisited w cur
     else
     shortespospathto graph goal pvisited nvisited max cur
       
shortespospathto:: Graph -> String -> [String] -> [String] -> Double -> String -> Maybe Double
shortespospathto graph goal pvisited nvisited max cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    case ffind goal posdep of
                                    Just x -> if x > max
                                                 then
--                                                  trace ("found "++(show x)++"\n") $
                                                 Just x
                                                 else
--                                                  trace ("found "++(show max)++"\n") $
                                                 Just max
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
  trace ("try neg node "++cur++" weight "++(show w)++"\n") $
  if w > max
     then
--      trace ("update max "++(show w)++"\n") $
     shortesnegpathto graph goal pvisited nvisited w cur
     else shortesnegpathto graph goal pvisited nvisited max cur
     
shortesnegpathto:: Graph -> String -> [String] -> [String] -> Double -> String -> Maybe Double
shortesnegpathto graph goal pvisited nvisited max cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    case ffind goal negdep of
                                    Just x -> if x > max
                                                 then
--                                                  trace ("found "++(show x)++"\n") $
                                                 Just x
                                                 else
--                                                  trace ("found "++(show max)++"\n") $
                                                 Just max
                                    _ ->
                                      let new_nvisited = (name:nvisited)
                                          mini = (maybe_minimum ( (map (shortenegpathto graph goal pvisited new_nvisited max) (wo posdep new_nvisited))
                                                                ++(map (shortepospathto graph goal pvisited new_nvisited max) (wo negdep pvisited)))
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


transwesd:: Graph -> Double -> [ Edge]
transwesd graph alpha =
  let
--       check = hasnegCycle graph
--       nodes = Map.keys graph
      candidates = getCandidateEdges graph alpha
  in
  catMaybes candidates

    
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
  trace ("check_node "++nodei++"\n"
  ) $
  case Map.lookup nodei graph of 
  Just (Node name posdep negdep) ->
    trace ("check_node "++name++" "++(show posdep)++" "++(show negdep)++"\n" ) $
    let e1 = map (check_pos graph alpha nodei) posdep
        e2 = map (check_neg graph alpha nodei) negdep
    in e1++e2
  Nothing -> []


  
check_pos:: Graph -> Double ->  String -> (String,Double) -> Maybe Edge
check_pos graph alpha nodei (nodek,weight) =
  trace ("  checkp "++(show nodei)++"->"++(show nodek)++" "++(show weight)++"\n" ) $
  case Map.lookup nodei graph of
       Just (Node name posdep negdep) -> let pdep = delete (nodek,weight) posdep
                                             ndep = negdep
                                             weights = (map second pdep) ++ (map second ndep)
                                             spaths = (map (shortepospathto graph nodek [] [] 0) pdep) ++ (map (shortenegpathto graph nodek [] [] 0) ndep)
                                             bothscores = zip weights spaths
                                         in
--                                          trace ( " show scores" ++"\n"
--                                          ++"weights"++ (show weights) ++"\n"
--                                          ++"spaths"++ (show spaths) ++"\n"
--                                          ) $
                                         if (test alpha weight bothscores)
                                         then Just (Edge nodei nodek True weight)
                                         else Nothing
       _ -> Nothing

check_neg:: Graph -> Double ->  String -> (String,Double) -> Maybe Edge
check_neg graph alpha nodei (nodek,weight) =
  trace ("  checkn "++(show nodei)++"->"++(show nodek)++" "++(show weight)++"\n" ) $
  case Map.lookup nodei graph of
       Just (Node name posdep negdep) -> let pdep = posdep
                                             ndep = delete (nodek,weight) negdep
                                             weights = (map (second) pdep) ++ (map (second) ndep)
                                             spaths = (map (shortenegpathto graph nodek [] [] 0) pdep) ++ (map (shortepospathto graph nodek [] [] 0) ndep)
                                             bothscores = zip weights spaths
                                         in
--                                          trace ( " show scores" ++"\n"
--                                          ++"weights"++ (show weights) ++"\n"
--                                          ++"spaths"++ (show spaths) ++"\n"
--                                          ) $
                                         if (test alpha weight bothscores)
                                         then Just (Edge nodei nodek False weight)
                                         else Nothing                                         
       _ -> Nothing
                                         
second (node,weight) =  Just weight


test:: Double -> Double -> [(Maybe Double,Maybe Double)] -> Bool
test alpha k [] = False
test alpha k ((Just a,Just b):xs) =
  trace ( "     test alpha k= "++(show alpha)++" "++(show k)++" vs\n"
  ++"weight="++(show a)++" spath= "++(show b)++"\n"
  ) $
  if (((alpha*k)> a) && ((alpha*k)> b))
     then True
     else test alpha k xs
test alpha k (x:xs) = test alpha k xs
     
     

     

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

type Motif = (String,String,String)

get_motif1:: Graph -> [Motif]
get_motif1 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt1 combies
  in
  motifs

get_motif2:: Graph -> [Motif]
get_motif2 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt2 combies
  in
  motifs

get_motif3:: Graph -> [Motif]
get_motif3 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt3 combies
  in
  motifs

get_motif4:: Graph -> [Motif]
get_motif4 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt4 combies
  in
  motifs

get_motif5:: Graph -> [Motif]
get_motif5 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt5 combies
  in
  motifs

get_motif6:: Graph -> [Motif]
get_motif6 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt6 combies
  in
  motifs

get_motif7:: Graph -> [Motif]
get_motif7 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt7 combies
  in
  motifs

get_motif8:: Graph -> [Motif]
get_motif8 graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motifs = mapMaybe checkmt8 combies
  in
  motifs  

get_motifs:: Graph -> ([Motif] -- c1 ffl
                      ,[Motif] -- c2 ffl
                      ,[Motif] -- c3 ffl
                      ,[Motif] -- c4 ffl
                      ,[Motif] -- 11 ffl
                      ,[Motif] -- 12 ffl
                      ,[Motif] -- 13 ffl
                      ,[Motif]) -- 14 ffl                    
get_motifs graph =
 let nodes = Map.elems graph
     combies = [(n1,n2,n3) | n1 <- nodes, n2 <- nodes, n3 <- nodes, n1/=n2, n1/=n3, n2/=n3 ]
     motif1 = mapMaybe checkmt1 combies
     motif2 = mapMaybe checkmt2 combies
     motif3 = mapMaybe checkmt3 combies
     motif4 = mapMaybe checkmt4 combies
     motif5 = mapMaybe checkmt5 combies
     motif6 = mapMaybe checkmt6 combies
     motif7 = mapMaybe checkmt7 combies
     motif8 = mapMaybe checkmt8 combies
  in
  (motif1
  ,motif2
  ,motif3
  ,motif4
  ,motif5
  ,motif6
  ,motif7
  ,motif8)

      
checkmt1:: (Node,Node,Node) -> Maybe Motif
checkmt1 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes pdx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes pdy))
     then Just (nx,ny,nz)
     else Nothing


checkmt2:: (Node,Node,Node) -> Maybe Motif
checkmt2 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes ndx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes pdy))
     then Just (nx,ny,nz)
     else Nothing

checkmt3:: (Node,Node,Node) -> Maybe Motif
checkmt3 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes ndx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes ndy))
     then Just (nx,ny,nz)
     else Nothing

checkmt4:: (Node,Node,Node) -> Maybe Motif
checkmt4 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes pdx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes ndy))
     then Just (nx,ny,nz)
     else Nothing

checkmt5:: (Node,Node,Node) -> Maybe Motif
checkmt5 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes pdx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes ndy))
     then Just (nx,ny,nz)
     else Nothing
  
checkmt6:: (Node,Node,Node) -> Maybe Motif
checkmt6 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes ndx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes ndy))
     then Just (nx,ny,nz)
     else Nothing

checkmt7:: (Node,Node,Node) -> Maybe Motif
checkmt7 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes ndx)) && (elem ny (getnodes pdx)) && (elem nz (getnodes pdy))
     then Just (nx,ny,nz)
     else Nothing

checkmt8:: (Node,Node,Node) -> Maybe Motif
checkmt8 ((Node nx pdx ndx), (Node ny pdy ndy), (Node nz pdz ndz)) =
  if (elem nz (getnodes pdx)) && (elem ny (getnodes ndx)) && (elem nz (getnodes pdy))
     then Just (nx,ny,nz)
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

get_ds :: Graph ->  String -> [String]
get_ds g s =
--   trace ("getds: "++ s ++"\n") $
  get_downstream g [] s


get_downstream1:: Graph -> [String] -> [String] -> [String]
get_downstream1 g found [] = found
get_downstream1 g found (n:ns) =
  let found'= get_downstream g found n in
  get_downstream1 g found' ns
  
   
get_downstream:: Graph -> [String] -> String -> [String]
get_downstream g found s =
--   trace ("getds: "++ s ++"\n") $
    case Map.lookup s g of
      Nothing -> found
      Just (Node name pos neg) -> let downstream = (((getnodes pos) ++ (getnodes neg)) \\ found)
                                      found' = found++ (nub downstream)
                                  in
                                  get_downstream1 g found' downstream


get_pos:: Graph ->  String -> [String]
get_pos g  s =
    case Map.lookup s g of
      Nothing -> []
      Just (Node name pos neg) -> getnodes pos

get_neg:: Graph ->  String -> [String]
get_neg g  s =
    case Map.lookup s g of
      Nothing -> []
      Just (Node name pos neg) -> getnodes neg
                                        
check_properties:: [(String,String)] -- map Node Property
                -> [(String,Int)]    -- acummulator how often a property
                -> [String]          -- list of nodes
                -> [(String,Int)]    -- how many nodes have a property
                
check_properties m acc [] = acc
check_properties m acc (n:ns) =
  let props = [ prop | (node,prop) <- m, n==node]
      acc1  = [ (prop,i) | (prop,i) <- acc, not (elem prop props) ]
      acc2  = [ (prop,i+1) | (prop,i) <- acc, elem prop props ]
  in
  check_properties m (acc1++acc2) ns
