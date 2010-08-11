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

-------------------------- < little helper > ----------------------------------
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


-- builds a stringlist from a string. The new list element will beginn after
-- the given char.
stringListBreakAt :: String -> Char -> [String]
stringListBreakAt string char =
    lines $ map (\x -> if x == char then '\n' else x) string


-- turns a tuplestring to tuples. Get ["(1,1)(2,2).."] and returns
-- [(1,1),(2,2),..] 
createPosTuple :: String -> [Pos]
createPosTuple s = 
    map (\str -> (read (fst $ break (==',') str) :: Int,
                  read (tail $ snd $ break (==',') str) :: Int))
        $ lines withoutBrakets
    where withoutBraketFst = filter (\x -> x /= '(') s
          withoutBrakets = map (\x -> if x ==')' then '\n' else x)
                               withoutBraketFst


-------------------------- < aStar > ------------------------------------------
-- the start for the aStat algorithm. It takes the starting position, the
-- destination and the static cells of the city. 
-- It beginns with the first node at the current position with the priority of
-- zero. The closedlist is still empty
aStar :: Pos -> Pos -> [[Cell]] -> [Pos]
aStar position destination staticC =
    aStar' [(0, (Node position []))] [] destination staticC


aStar' :: [(Int, Tree Pos)] -> [Pos] -> Pos -> [[Cell]] -> [Pos]
aStar' [] _ _ _ = error "No path found for one car!"
aStar' openList closedList destination staticC = 
    -- first it searches the lowes priority and removed it from the open list
    let (i, currentNode) = minimumBy orderTreeList openList
        newOpenList = openList\\[(i,currentNode)]
    -- second it puts this node to the closed list
        newClosedList = closedList++[rootLabel currentNode]
    in 
    -- after that it checks, if the current node is the destination
    if destination == (rootLabel currentNode)
       -- if it's the destinatino, then the path will be rebuild 
       then buildPath currentNode
       -- if not, then aStar' will be executed again with the new openList
       -- from the expandNode function and the new closedList
       else aStar' (expandNode (i, currentNode)
                               newOpenList
                               newClosedList
                               staticC
                               destination) 
                  newClosedList
                  destination
                  staticC

-- it checks every next node from the current node and adds them to the open
-- list, if the next node is new or updated this node if a better way was
-- founded.
-- Every successor knows the node, wich has a pointer to the successor. 
-- So we can rebuild the way from the destination to the source possition.
expandNode :: (Int, Tree Pos)   ->
              [(Int, Tree Pos)] -> 
              [Pos]             -> 
              [[Cell]]          -> 
              Pos               ->
              [(Int, Tree Pos)]
expandNode (g,node) openList closedList staticC destination = 
    -- for each successor of the current node it will be checked if
    nub $concatMap (\pos -> do
        -- the successor is in the list
        let nodeInList = filter (\(_, Node posN _) -> posN==pos)
                                openList
        -- then it will be removed from the openList
            newOpenList = openList\\nodeInList
        -- the old priority will be known as fOld
            (fOld,_) = head nodeInList
        -- if the successor is in closed list then do nothing
        if elem pos closedList
            then openList
        -- else if the successor is in the open list
            else if not (null $nodeInList)
        -- then checks, if the old priority is greater than the new one
                    then if f pos > fOld
        -- if its true, then do nothing, else update the successor
        -- with the lower priority
                         then openList
                         else newOpenList++[(f pos,Node pos [node])]
        -- else put it to the open list
                    else openList++[(f pos,Node pos [node])]
        )
        successors
    
    where position = rootLabel node
          successors = nextRoad $getCell staticC position
    -- the new priority will be build with the priority from the current node
    -- (g) plus the costs to go from the current node to the successor (1) and 
    -- plus the heuristic costs to go from the successor to the destination
          f :: Pos -> Int
          f pos = g+1+heuristic pos destination

    
-- It's a very simple heuristic to appreciate the costs to go from the current
-- position to the destination position. It uses the sum of the absolute 
-- value from the difference between the current and the destination x and y.
heuristic :: Pos -> Pos -> Int
heuristic (xa,ya) (xd,yd) = 
    (abs $xa-xd)+ (abs $ya-yd)
    
-- it rebuilds the path from the destination to the source position
buildPath :: Tree Pos -> [Pos]
buildPath (Node destination []) = [destination]
buildPath node =
    destination:(buildPath (head $ subForest node))
    where destination = rootLabel node

-- a function to order the nodes by using the priorities
orderTreeList :: (Ord a) => (a, b) -> (a, b) -> Ordering
orderTreeList (i1, _) (i2, _)
    | i1 < i2 = LT
    | i1 == i2 = EQ
    | i1 > i2 = GT
orderTreeList _ _ = error "Error in orderTreeList"


