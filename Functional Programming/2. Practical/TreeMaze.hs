-- Module to define the type of a maze 
module TreeMaze (
   Maze, 
   makeMaze, -- :: Size -> [Wall] -> Maze
   hasWall,  -- :: Maze -> Place -> Direction -> Bool
   sizeOf    -- :: Maze -> Size
   )
  where

import Geography
import Data.List

data Btree a = Empty | Branch a (Btree a) (Btree a)

data Maze = TreeMaze Size  (Btree Place) (Btree Place)(Btree Place)(Btree Place)

makeMaze :: Size -> [Wall] -> Maze

makeMaze (x,y) walls =  TreeMaze (x,y) souths norths easts wests
                      where alloftheWalls= allWalls (x,y) walls
                            lst = wallToPlaces alloftheWalls
                            souths= placeToBtrees (lst !! 0)
                            norths= placeToBtrees (lst !! 1)
                            easts= placeToBtrees(lst !! 2)
                            wests= placeToBtrees(lst !! 3)


hasWall :: Maze -> Place -> Direction -> Bool
hasWall (TreeMaze _ souths norths easts wests) pos d | d == S = binSearch pos souths
                                                     | d == N = binSearch pos norths
                                                     | d == E = binSearch pos easts
                                                     | d == W = binSearch pos wests

binSearch :: (Ord a) => a -> Btree a -> Bool
binSearch elem Empty = False
binSearch elem (Branch a tree1 tree2) | elem==a = True
                                      |  elem < a = binSearch elem tree1
                                      |  elem > a = binSearch elem tree2


reflect :: Wall -> Wall
reflect ((i,j), d) = (move d (i,j), opposite d)

allWalls :: Size -> [Wall]  -> [Wall]  
allWalls (x,y) walls =
  let boundaries = -- the four boundaries
        [((0,j),   W) | j <- [0..y-1]] ++ -- westerly boundary
        [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
        [((i,0),   S) | i <- [0..x-1]] ++ -- southerly boundary
        [((i,y-1), N) | i <- [0..x-1]]    -- northerly boundary
      allWalls = walls ++ boundaries ++ map reflect (walls ++ boundaries)
 in allWalls    




placeToBtrees :: [Place] -> Btree Place
placeToBtrees places | length(places)==0 = Empty
                     | otherwise = Branch (sortedPlaces !! midLength) ( placeToBtrees (take midLength sortedPlaces )) ( placeToBtrees (drop (midLength+1) sortedPlaces ))
                          where sortedPlaces = sort places 
                                midLength | (length places) `mod` 2 ==0 = (length places) `div` 2
                                        | otherwise = ((length places) -1 ) `div` 2

-- The following function returns the size of a maze:

sizeOf :: Maze -> Size
sizeOf (TreeMaze size _ _ _ _) = size

wallToPlaces :: [Wall] -> [[Place]]
wallToPlaces [] = [[],[],[],[]]
wallToPlaces ((place,direction):walls) | direction==S = [ place:((wallToPlaces walls)!!0),((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]
				       | direction==N = [ ((wallToPlaces walls)!!0), place:((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]
				       | direction==E = [ ((wallToPlaces walls)!!0), ((wallToPlaces walls)!!1),place:((wallToPlaces walls)!!2),((wallToPlaces walls)!!3) ]        				                                       
		| direction==W = [ ((wallToPlaces walls)!!0), ((wallToPlaces walls)!!1),((wallToPlaces walls)!!2),place:((wallToPlaces walls)!!3) ]