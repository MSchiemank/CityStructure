module Run2 where
import Parse
import Data.List (nub, (\\), minimumBy)
import Data.Tree


-- The nextStep is first to generate a new dynamic list, with the moving cars
-- on the next point in the City and the switching signals.Out of this
-- a new city will be generated.
nextStep :: City -> City
nextStep city = 
    City {width  = getCityWidth  city,
          height = getCityHeight city,
          static = getCityStatic city,
          dynamic = allCarsWithoutTwoOnOneCell}
    
    where 
          dyn = getCityDynamic city
          stat = getCityStatic city
          -- extract the cars with blinked house
          withoutCarsOnDest = filter (\(pos,_) -> pos /= (-1,-1)) dyn
          -- build the next way of all cars
          allNextCars = map (nextCarsAndSignals stat
                                                withoutCarsOnDest) 
                            withoutCarsOnDest 
          -- cars with position (-1,-1) are on their destination and 
          -- are filtered out
          destCars = filter (\(pos,_) -> pos == (-1,-1)) allNextCars
          withDoubleCars = allNextCars\\destCars
          --returns a list like that: [[pos1,pos2],[pos3],[pos4]...]
          searchDoubleForEachCell =
            map (\(pos1,_) -> carDoubleSearch pos1) withDoubleCars 

          carDoubleSearch :: Pos -> [(Pos,Cell)]
          carDoubleSearch pos1 = 
            filter (\(pos,_) -> pos == pos1) withDoubleCars
          --extract the double cars on one Cell
          extractDouble = 
            nub $ filter (\x -> length x >1) searchDoubleForEachCell
          --this will correct the two cars on one cell phenomenon
          allCarsWithoutTwoOnOneCell =
             concat noTwoCarsOnPlace ++ 
             noDoubleCarsInside ++
             destCars
          noTwoCarsOnPlace = map rightBeforeLeft extractDouble
          noDoubleCarsInside = withDoubleCars\\(concat extractDouble) 


                      
-- returns the old position of the first car on the cell, the other car
-- moves on will be used, if two cars are on one cell.
rightBeforeLeft :: [(Pos,Cell)] -> [(Pos,Cell)]
rightBeforeLeft cars = 
    if (length cars)==2 
        then whoIsTheRightOfTheCar firstCar secondCar :
             whoIsTheRightOfTheCar secondCar firstCar : []
        else error $"More ore less than two cars on one roadcell"
                    ++" in rightBeforeLeft!"
    where
          firstCar = cars!!0
          secondCar = cars!!1
 

whoIsTheRightOfTheCar :: (Pos,Cell) -> (Pos,Cell) -> (Pos,Cell)
whoIsTheRightOfTheCar car1 car2 = 
    -- first time is, that a car comes from the top
    if xCar1==oldXCar1 
        then if yCar1>oldYCar1
                then showRight (xCar1-1,yCar1) car1 car2
    -- second time is, that a car comes from the bottom
                else showRight (xCar1+1, yCar1) car1 car2
    -- third time is, that a car comes from the left side
        else if xCar1>oldXCar1
                then showRight (xCar1, yCar1+1) car1 car2
    -- fourth time is, that a car comes from the right side
                else showRight (xCar1, yCar1-1) car1 car2
    
    where 
          oldPosFirstCar = (\(Car _ _ old _) -> head $reverse old) $snd car1
          posCar1 = fst car1
          xCar1 = fst posCar1
          yCar1 = snd posCar1
          oldXCar1 = fst oldPosFirstCar
          oldYCar1 = snd oldPosFirstCar



-- shows on the right side of a car, if there is another car.
showRight :: Pos -> (Pos, Cell) 
            -> (Pos, Cell) -> (Pos, Cell)
showRight rightPos car1 car2 =
    if rightPos==oldPosCar2
       then (returnOldPos car1, remLastPos car1)
       else car1
    
    where 
          oldPosCar2 = (\(Car _ _ old _) -> head $reverse old) $snd car2



returnOldPos :: (Pos,Cell) -> Pos
returnOldPos (_,Car _ _ oldPath _) = head $reverse oldPath
returnOldPos _ = error "Must be a car cell in returnOldPos!"

remLastPos :: (Pos,Cell) -> Cell
remLastPos (_,Car idC destC oldPath col) =
    Car idC destC (reverse $ tail $ reverse oldPath) col
remLastPos _ = error "Must be a car cell in remLastPos!"




-- The cars look for the next field in the street cell. If there will
-- be 2 next fields, then it will look, which field is the nearest
-- to the target. There for are the wights. If the wight is equal, then
-- the next but one fields will be looked for. And this fields have also
-- a next field, then the wights for this will be created and the lower wight
-- will elected the real next step. After the car has reached the destination,
-- it will be removed from the city.
nextCarsAndSignals :: [[Cell]] -> [(Pos,Cell)] -> (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals staticC dynamicC (pos, cell) =
    case cell of 
         { (Signal _ _ _ _ _ _) -> signalStatus (pos, cell);
           (Car _ _ _ _)        -> checkSignal staticC dynamicC pos cell;
           (_)                  -> error $"Must be a signal or car cell in"
                                          ++" nextCarsAndSignals!"
         }


-- switches the signals
signalStatus :: (Pos,Cell) -> (Pos,Cell)
signalStatus (pos, Signal idS stat step remainingStep wW wA) =
                if remainingStep == 1
                   then (pos, Signal { ident         = idS,
                                       status        = not stat,
                                       stepToWait    = step,
                                       remainingSteps= step,
                                       workWith      = wW,
                                       against       = wA})
                   else (pos, Signal { ident         = idS  ,
                                       status        = stat,
                                       stepToWait    = step,
                                       remainingSteps= (remainingStep-1),
                                       workWith      = wW,
                                       against       = wA})
signalStatus (_,_) = error "Must be a signal cell in signalStatus!"


-- Checks, if the next or the next but one roadpiece is a junction.
-- If thats true, the status of the signal will be checked.
-- If signal shows red, then the car will remain on the position, otherwise
-- it will move forward.
checkSignal ::  [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell ->  (Pos, Cell)
checkSignal stat dyn (x,y) cell = 
    if length nextButOnePos > 1 
       then if length nearestSignal > 0
               then if and $ map (\(_,(Signal _ statusS _ _ _ _)) -> statusS) 
                                 nearestSignal
                       then carStep stat dyn (x,y) cell
                       else ((x,y),cell)
               else carStep stat dyn (x,y) cell
       else carStep stat dyn (x,y) cell
    
    where 
          nextPos = head(nextRoad (getCell stat (x,y)))
          nextButOnePos = nextRoad (getCell stat nextPos)
          allSignals = filter (\(_,cell1) -> case cell1 of 
                                 {(Signal _ _ _ _ _ _) -> True;
                                  _                    -> False}) dyn
          nearestSignal = filter (\((x1,y1),_) ->
                                 x1==x && (y1==y-1 || y1==y+1) ||
                                 y1==y && (x1==x-1 || x1==x+1)
                                 ) allSignals


getCell :: [[Cell]] -> (Pos) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)



-- Remove the car, if its on the destination or in the neighborhood.
carStep :: [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell -> (Pos,Cell)
carStep staticL dyn (xa,ya) (Car idC (xd,yd) pathOld col) = 
    if (xa,ya) == (xd,yd) || 
       xa==xd && (ya==yd-1 || ya==yd+1) || 
       ya==yd && (xa==xd-1 || xa==xd+1)
       then
         --car is on his destination
         ((-1,-1), Car {ident=0, dest=(xd,yd), iWasThere=[], colour=col})
       else if length carOnNext > 0    --if a car is on the next field
        --checks, if a deadlock occures for 5 times
               then if oldCell == (xa,ya) && length carOnOtherNext <= 0
                -- shows, if it has waited 5 times and if the otherNext
                -- field is not occupied and goes on.
                       then (otherNext, Car idC 
                                            (xd,yd) 
                                            (pathOld++[(xa,ya)])
                                            col)
                --else it will remain on the current place
                       else ((xa,ya), Car idC
                                          (xd,yd)
                                          (pathOld++[(xa,ya)])
                                          col)
               else --otherwise it will move on
                    (next, Car idC 
                               (xd,yd) 
                               newOldNext
                               col)
    
    where next = nextField staticL (getCell staticL (xa,ya)) (xd,yd) (xa,ya)
          nextRoadFromCell = nextRoad (getCell staticL (xa,ya))
          otherNext = if length nextRoadFromCell >1
                         then head $nextRoadFromCell\\[next]
                         else next
          carOnNext = filter (\(pos,_) -> pos==next) dyn
          carOnOtherNext = filter (\(pos,_) -> pos==otherNext) dyn
          oldCell = if (length pathOld) > 4
                        then (reverse pathOld)!!4
                        else (-1,-1)
          -- remove (xa,ya) for the first time it 
          -- appears and put it at the end of the list!
          newOldNext = nub ((pathOld\\[(xa,ya)])++[(xa,ya)])

                            
carStep _ _ _ _ = error "Must be a car cell in carStep!"



---------------------------- < pathfinding > ----------------------------------
-- with the a-star algorithm.
nextField :: [[Cell]] -> Cell -> Pos -> Pos -> Pos
nextField staticC (Road _ _ next) destination position = 
    if (length next > 1)
        then if length path > 1 
                then head $tail $reverse path
                else head path
        else head next
    where path = aStar [(0, (Node position []))] [] destination staticC
nextField _ _ _ _ = error "Must be a road cell in nextField!"


aStar :: [(Int, Tree Pos)] -> [Pos] -> Pos -> [[Cell]] -> [Pos]
aStar [] _ _ _ = error "No path found for one car!"
aStar openList closedList destination staticC = 
    let (i, currentNode) = minimumBy orderTreeList openList
        newOpenList = openList\\[(i,currentNode)]
        newClosedList = closedList++[rootLabel currentNode]
    in 
    if destination == (rootLabel currentNode)
       then buildPath currentNode
       else aStar (expandNode (i, currentNode)
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


-------------------------------------------------------------------------------

-- the house at the finishing point to show it with a circle
-- If no house is at that position, it will choose the destination 
-- position itself!
filterHouseAt :: [[Cell]] -> Pos -> Pos
filterHouseAt staticC (x,y) = 
    if null houses 
       then (x,y)
       else (\(position,_) -> position) $head houses
    
    where 
          lX = length $ staticC!!0
          lY = length staticC
          array = [(x',y') |  y' <- [1..lY], x' <- [1..lX]]
          cell = zip array $concat staticC
          bordering = filter (\(pos,_) -> pos==(x-1,y) ||
                                          pos==(x+1,y) ||
                                          pos==(x,y-1) ||
                                          pos==(x,y+1)) cell
          houses = filter (\(_,bordCell) -> case bordCell of
                            {Building _ _ -> True;
                     _                    -> False}) bordering


