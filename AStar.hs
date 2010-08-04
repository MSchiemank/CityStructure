module AStar where

import Data.List (nub, (\\), minimumBy)
import Data.Tree

--Datatype setting
data Cell = Road    { ident         :: Int,
                      name          :: String,
                      nextRoad      :: [Pos]}
          | Building{ ident         :: Int,
                      name          :: String}
          | Signal  { ident         :: Int,
                      status        :: Bool,
                      stepToWait    :: Int,
                      remainingSteps:: Int,
                      workWith      :: [Pos],
                      against       :: [Pos]}
          | Car     { ident         :: Int,
                      dest          :: Pos,
                      path          :: [Pos],
                      iWasThere     :: [Pos],
                      colour        :: RGB}
          | Empty {}
     deriving (Eq, Show)

type Pos = (Int, Int)
type RGB = (Double, Double, Double)

data City = City { width              :: Int,
                   height             :: Int,
                   static             :: [[Cell]],
                   dynamic            :: [(Pos,Cell)]}
     deriving (Eq, Show)

------------------- < little helpers > ----------------------------------------
getCell :: [[Cell]] -> (Pos) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)

--returns the contents of the structure
getCityStatic :: City -> [[Cell]]
getCityStatic (City _ _ stat _) = stat

getCityDynamic :: City -> [(Pos,Cell)]
getCityDynamic (City _ _ _ dyn) = dyn

getCityWidth :: City -> Int
getCityWidth (City widthCity _ _ _) = widthCity

getCityHeight :: City -> Int
getCityHeight (City _ heightCity _ _) = heightCity




aStar :: Pos -> Pos -> [[Cell]] -> [Pos]
aStar position destination staticC =
    aStar' [(0, (Node position []))] [] destination staticC


aStar' :: [(Int, Tree Pos)] -> [Pos] -> Pos -> [[Cell]] -> [Pos]
aStar' [] _ _ _ = error "No path found for one car!"
aStar' openList closedList destination staticC = 
    let (i, currentNode) = minimumBy orderTreeList openList
        newOpenList = openList\\[(i,currentNode)]
        newClosedList = closedList++[rootLabel currentNode]
    in 
    if destination == (rootLabel currentNode)
       then buildPath currentNode
       else aStar' (expandNode (i, currentNode)
                               newOpenList
                               newClosedList
                               staticC
                               destination) 
                  newClosedList
                  destination
                  staticC


expandNode :: (Int, Tree Pos)   ->
              [(Int, Tree Pos)] -> 
              [Pos]             -> 
              [[Cell]]          -> 
              Pos               ->
              [(Int, Tree Pos)]
expandNode (g,node) openList closedList staticC destination = 
    nub $concat $map (\pos -> do
        let nodeInList = filter (\(_, Node posN _) -> posN==pos)
                                openList
            newOpenList = openList\\nodeInList
            (fOld,_) = head nodeInList
        if elem pos closedList
            then openList
            else if not (null $nodeInList)
                    then if f pos > fOld
                         then openList
                         else newOpenList++[(f pos,Node pos [node])]
                    else openList++[(f pos,Node pos [node])]
        )
        successors
    
    where position = rootLabel node
          successors = nextRoad $getCell staticC position
          f :: Pos -> Int
          f pos = g+1+heuristic pos destination

    

heuristic :: Pos -> Pos -> Int
heuristic (xa,ya) (xd,yd) = 
    (if xd<xa then xa-xd else xd-xa)+(if yd<ya then ya-yd else yd-ya)

buildPath :: Tree Pos -> [Pos]
buildPath (Node destination []) = [destination]
buildPath node =
    destination:(buildPath (head $ subForest node))
    where destination = rootLabel node


orderTreeList :: (Ord a) => (a, b) -> (a, b) -> Ordering
orderTreeList (i1, _) (i2, _)
    | i1 < i2 = LT
    | i1 == i2 = EQ
    | i1 > i2 = GT
orderTreeList _ _ = error "Error in orderTreeList"


