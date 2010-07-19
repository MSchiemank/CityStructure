module Run where
import Parse
import Data.List

{- The nextStep is first to generate a new dynamic list, with the moving cars on the next
   point in the City and the switching signals.Out of this a new city will be generated.-}
nextStep :: City -> City
nextStep city =
    City {width  = getCityWidth  city,
          height = getCityHeight city,
          static = getCityStatic city,
          dynamic = allCarsWithoutTwoOnOneCell}
    where 
          dyn = getCityDynamic city
          -- build the next way of all cars
          allNextCars = (map (nextCarsAndSignals (getCityStatic city) dyn) dyn) 
          -- cars with position (-1,-1) are on their destination and 
          -- are filtered out
          withDoubleCars = filter (\(pos,_) -> pos /= (-1,-1)) allNextCars
          --returns a list like that: [[pos1,pos2],[pos3],[pos4]...]
          searchDoubleForEachCell = map (\(pos1,_) -> carDoubleSearch pos1) withDoubleCars 
          carDoubleSearch :: Pos -> [(Pos,Cell)]
          carDoubleSearch pos1 = filter (\(pos,_) -> pos == pos1) withDoubleCars
          --extract the double cars on one Cell
          extractDouble = nub $ filter (\x -> length x >1) searchDoubleForEachCell
          --this will correct the two cars on one cell phenomenon
          allCarsWithoutTwoOnOneCell = concat noTwoCarsOnPlace ++ noDoubleCarsInside
          noTwoCarsOnPlace = map rightBeforeLeft extractDouble
          noDoubleCarsInside = withDoubleCars\\(concat extractDouble) 


                      
-- returns the old position of the first car on the cell, the other car moves on
-- will be used, if two cars are on one cell.
rightBeforeLeft :: [(Pos,Cell)] -> [(Pos,Cell)]
rightBeforeLeft cars = 
    if (length cars)==2 
        then whoIsTheRightOfTheCar firstCar secondCar :
             whoIsTheRightOfTheCar secondCar firstCar : []
        else error "More ore less than two cars on one roadcell in rightBeforeLeft!"
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




{- The cars look for the next field in the street cell. If there will be 2 next 
   fields, then it will look, which field is the nearest to the target. There for 
   are the wights. If the wight is equal, then the next but one fields will be
   looked for. And this fields have also a next field, then the wights for this will
   be created and the lower wight will elected the real next step.
   After the car has reached the destination, it will be removed from the city.-}
nextCarsAndSignals :: [[Cell]] -> [(Pos,Cell)] -> (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals staticC dynamicC (pos, cell) =
    case cell of 
         { (Signal _ _ _ _ _ _) -> signalStatus (pos, cell);
           (Car _ _ _ _)        -> checkSignal staticC dynamicC pos cell;
           (_)                  -> error "Must be a signal or car cell in nextCarsAndSignals!"
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
    if length nextButOnePos > 1 || length nextButTwoPos > 1
       then if length nearestSignal > 0
               then if and $ map (\(_,(Signal _ statusS _ _ _ _)) -> statusS) nearestSignal
                       then carStep stat dyn (x,y) cell
                       else ((x,y),cell)
               else carStep stat dyn (x,y) cell
       else carStep stat dyn (x,y) cell
    where 
          nextPos = head((\(Road _ _ next) -> next) (getCell stat (x,y)))
          nextButOnePos = (\(Road _ _ next) -> next) (getCell stat nextPos)
          nextButTwoPos = map (\(Road _ _ next) -> next) nextButTwoRoads 
          nextButTwoRoads = map (getCell stat) nextButOnePos
          allSignals = filter (\(_,cell1) -> case cell1 of 
                                 {(Signal _ _ _ _ _ _) -> True;
                                  _                    -> False}) dyn
          nearestSignal = filter (\((x1,y1),_) -> x1==x && (y1==y-1 || y1==y+1) ||
                                                  y1==y && (x1==x-1 || x1==x+1)) allSignals


getCell :: [[Cell]] -> (Pos) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)



-- Remove the car, if its on the destination or in the neighborhood.
carStep :: [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell -> (Pos,Cell)
carStep staticL dyn (xa,ya) (Car idC (xd,yd) pathOld col) = 
    if (xa,ya) == (xd,yd) || 
       xa==xd && (ya==yd-1 || ya==yd+1) || 
       ya==yd && (xa==xd-1 || xa==xd+1)
       then ((-1,-1), Car {ident=0, dest=(-1,-1), iWasThere=[], colour=col}) --car is on his destination
       else if length (filter (\(pos,_) -> pos==next) dyn) > 0    --if a car is on the next field
               then if length (filter (\(pos,_) -> pos==otherNext) dyn) > 0
                        then ((xa,ya), Car idC (xd,yd) pathOld col)                --then it will remain on the current place
                        else (otherNext, Car idC (xd,yd) (nub (pathOld++[(xa,ya)])) col)
               else if next == (-1,-1)
                       then ((xa,ya), Car idC (xd,yd) [] col)           --car has lost it's way
                       else (next, Car idC (xd,yd) (nub (pathOld++[(xa,ya)])) col)      --otherwise it will move on
    
    where next = nextField staticL (getCell staticL (xa,ya)) (xd,yd) pathOld
          nextRoadFromCell = nextRoad (getCell staticL (xa,ya))
          otherNext = if length nextRoadFromCell >1
                         then head $nextRoadFromCell\\[next]
                         else next

carStep _ _ _ _ = error "Must be a car cell in carStep!"

           

-- Check the length of the next cell list.
nextField :: [[Cell]]  -> Cell -> Pos -> [Pos] -> Pos
nextField staticC (Road _ _ next) destination oldWay = 
                 if (length possibleWays) /=1
                    then findWay staticC destination possibleWays
                    else head possibleWays
                 where possibleWays = next\\oldWay                    
nextField _ _ _ _ = error "Must be a road cell in nextField!"



findWay :: [[Cell]] -> Pos -> [Pos] -> Pos
findWay staticC destination list=
        findWayInCorrectDirection staticC destination (buildWeight destination list)
                     

-- builds the wight of the next cell
buildWeight :: Pos -> [Pos] -> [(Pos, Pos)]
buildWeight (xd,yd) list =
    map (\(x1,y1) -> (((if xd<x1 then x1-xd else xd-x1),(if yd<y1 then y1-yd else yd-y1)),(x1,y1))) list


{- This one decides which next cell will be used. If the weight from one
   of the way is more less than the other, that will be the next weight.
    If it's equal, then the wight form each next but one cell decides what
    way will be taken.-}
findWayInCorrectDirection :: [[Cell]] -> Pos -> [(Pos, Pos)] -> Pos
findWayInCorrectDirection _ _ [] = (-1,-1)
findWayInCorrectDirection staticC (xd,yd) (((wx1,wy1),(x1,y1)):((wx2,wy2),(x2,y2)):xs) =
    if (wx1+wy1) == (wx2+wy2)
       then if nextWeight1==nextWeight2
               then if (maximum [wx1,wy1] < maximum [wx2,wy2])
                        then (x1,y1)
                        else (x2,y2)
               else if nextWeight1<nextWeight2
                       then (x1,y1)
                       else (x2,y2)
       else (\(_,pos) -> pos) (minimum (map (\((wx,wy),pos) -> ((wx+wy),pos)) (((wx1,wy1),(x1,y1)):((wx2,wy2),(x2,y2)):xs)))
       where nextWeight1 = minimum (map (\((x,y),_) -> x+y) weight1)
             weight1 = buildWeight (xd,yd) nextCellNext1
             nextCellNext1 = (\(Road _ _ next) -> next) (getCell staticC (x1,y1))
             nextWeight2 = minimum (map (\((x,y),_) -> x+y) weight2)
             weight2 = buildWeight (xd,yd) nextCellNext2
             nextCellNext2 = (\(Road _ _ next) -> next) (getCell staticC (x2,y2))
findWayInCorrectDirection _ _ _ = error "To few ways in findWayInCorrectDirection!"



