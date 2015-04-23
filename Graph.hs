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
  transwesd
)
where

import Data.List (nub, (\\), delete )
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
  case Map.lookup source g of
  Just (Node name pdep ndep) -> if sign
                                then Map.insert source (Node source ((target,weight):pdep) ndep) g
                                else Map.insert source (Node source pdep ((target,weight):ndep)) g
  
  Nothing -> if sign
             then Map.insert source (Node source [(target,weight)] []) g
             else Map.insert source (Node source [] [(target,weight)]) g


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

flatten:: [[Maybe Edge]] -> [Maybe Edge]
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
     
     
     
     
     
     
     
     
     