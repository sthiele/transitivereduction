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
  Edge(..)
)
where

import Data.List (nub, (\\), delete )
import qualified Data.Map as Map
import Debug.Trace


data Node = Node { name::[Char]
                 , posdep::[([Char],Double)]
                 , negdep::[([Char],Double)]
}

instance Show Node where
  show (Node n pdep ndep) = n
instance Eq Node where
  (Node n1 pdep1 ndep1) == (Node n2 pdep2 ndep2) = n1==n2



data Edge = Edge { source::[Char]
                 , target::[Char]
                 , sign::Bool
                 , weight::Double
}
instance Show Edge where
  show (Edge s t sign w) = s++" -> "++t++" "++(show sign)++" "++(show w)



type Graph =  Map.Map ([Char]) (Node)

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
-- 
-- 
-- toGraph2:: [Edge] -> Graph -> Graph
-- toGraph2 [] g = g
-- toGraph2 ((Edge s t sign weight):es) g =
--   let g' =Map.insert s g
--       g'' Map.insert t g




getnodes:: [([Char],Double)] -> [[Char]]
getnodes [] = []
getnodes ((n,d):rest) = (n:(getnodes rest))


has_pathto:: Graph -> [Char] -> [[Char]] -> [Char] -> Bool
has_pathto graph goal visited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let dep = (getnodes posdep) ++ (getnodes negdep) in
                                    if elem goal dep
                                    then True
                                    else
                                      let new_visited = (name:visited) in
                                      or (map (has_pathto graph goal new_visited) (dep\\new_visited))
                  
  _ -> False
                                                

has_pospathto:: Graph -> [Char] -> [[Char]] -> [[Char]] -> [Char] -> Bool
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


has_negpathto:: Graph -> [Char] -> [[Char]] -> [[Char]] -> [Char] -> Bool
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


shortespathto:: Graph -> [Char] -> [[Char]] -> [Char] -> Maybe Int
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



       
shortespospathto:: Graph -> [Char] -> [[Char]] -> [[Char]] -> [Char] -> Maybe Int
shortespospathto graph goal pvisited nvisited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    if elem goal pdep
                                    then Just 1
                                    else
                                      let new_pvisited = (name:pvisited)
                                          mini = (maybe_minimum ( (map (shortespospathto graph goal new_pvisited nvisited) (pdep\\new_pvisited))
                                                                ++(map (shortesnegpathto graph goal new_pvisited nvisited) (ndep\\nvisited)))
                                                 )
                                      in
                                      case mini of
                                      Nothing -> Nothing
                                      Just x  -> Just (1+x)
                                                  
  _ -> Nothing

  
shortesnegpathto:: Graph -> [Char] -> [[Char]] -> [[Char]] -> [Char] -> Maybe Int
shortesnegpathto graph goal pvisited nvisited cur =
  case Map.lookup cur graph of
  Just (Node name posdep negdep) -> let pdep = (getnodes posdep)
                                        ndep = (getnodes negdep)
                                    in
                                    if elem goal ndep
                                    then Just 1
                                    else
                                    let new_nvisited = (name:nvisited)
                                        mini = (maybe_minimum ( (map (shortesnegpathto graph goal pvisited new_nvisited) (pdep\\pvisited))
                                                              ++(map (shortespospathto graph goal pvisited new_nvisited) (ndep\\new_nvisited)))
                                               )
                                    in
                                    case mini of
                                    Nothing -> Nothing
                                    Just x  -> Just (1+x)

  _ -> Nothing  
     
maybe_minimum:: [Maybe Int] -> Maybe Int
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


     
-- transwesd:: Graph -> Graph
-- transwesd g = 
--   let check = letnegCycle_exist g 
--   in
     
     
     
     
     
     
     
     
     