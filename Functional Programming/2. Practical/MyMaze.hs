module MyMaze (
   Maze, 
   makeMaze, -- :: Size -> [Wall] -> Maze
   hasWall,  -- :: Maze -> Place -> Direction -> Bool
   sizeOf    -- :: Maze -> Size
   )
  where

import Geography
import Data.List
data Maze =  AMaze Size [Place] [Place] [Place] [Place]

makeMaze :: Size -> [Wall] -> Maze
makeMaze size wall = helperMakeMaze size ( wallToPlaces wall) 

helperMakeMaze :: Size -> [[Place]] -> Maze
helperMakeMaze (x,y) [souths, norths, easts, wests] = AMaze (x,y) souths' norths' easts'  wests'
                                                       where souths'= souths ++ [(i,0) | i <- [0..x-1]] ++ [move N (i,j) |(i,j) <- norths] ++ [move N (i,y-1) | i <- [0..x-1]]
                                                             norths'= norths ++ [(i,y-1) | i <- [0..x-1]] ++ [move S (i,j) |(i,j) <- souths] ++ [move S (i,0) | i <- [0..x-1]]
                                                             easts'= easts ++ [(x-1,j) | j <- [0..y-1]] ++ [move W (i,j) |(i,j) <- wests] ++ [move W (0,j) | j <- [0..y-1]]
                                                             wests'= wests ++ [(0,j) | j <- [0..y-1]] ++ [move E (i,j) |(i,j) <- easts] ++ [move E (x-1,j) | j <- [0..y-1]]
                    
                    
wallToPlaces :: [Wall] -> [[Place]]
wallToPlaces [] = [[],[],[],[]]
wallToPlaces ((place,direction):walls) | direction==S = [ place:((wallToPlaces walls)!!0),((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]
				       | direction==N = [ ((wallToPlaces walls)!!0), place:((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]
				       | direction==E = [ ((wallToPlaces walls)!!0), ((wallToPlaces walls)!!1),place:((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]        				                                       
		| direction==W = [ ((wallToPlaces walls)!!0), ((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),place:((wallToPlaces walls)!!3) ]

hasWall :: Maze -> Place -> Direction -> Bool
hasWall (AMaze _ souths norths easts wests) pos d | d == S = pos `elem` souths
                                                  | d == N = pos `elem` norths
                                                  | d == E = pos `elem` easts
                                                  | d == W = pos `elem` wests
                                                  
                                                  

sizeOf :: Maze -> Size
sizeOf (AMaze size _ _ _ _) = size

{- 
(1.37 secs, 1,363,595,728 bytes) with the MyMaze

-}
