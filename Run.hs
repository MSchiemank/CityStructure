module Run where
import AStar
import Data.List (nub, (\\))


-------------------------- < nextStep > ---------------------------------------
-- The nextStep is first to generate a new dynamic list, with the moving cars
-- on the next position in the City and the switching signals. Out of this
-- a new city will be generated.
nextStep :: City -> City
nextStep city = 
    City {width  = getCityWidth  city,
          height = getCityHeight city,
          static = stat,
          dynamic = allCarsWithoutTwoOnOneCell}

    where 
          dyn = getCityDynamic city
          stat = getCityStatic city
          -- extract the cars with blinked house
          withoutCarsOnDest = filter (\(pos,_) -> pos /= (-1,-1)) dyn
          -- build the next way of all cars
          allNextCars = map (nextCarsAndSignals
                                 stat
                                 withoutCarsOnDest) 
                            withoutCarsOnDest 
          -- cars with position (-1,-1) are on their destination and 
          -- must be filtered out. If not, they don't appear in the dynamic
          -- cells and the house won't blink. They will be filtered out in 
          -- the carDoubleSearch function and causes an error.
          destCars = filter (\(pos,_) -> pos == (-1,-1)) allNextCars
          withDoubleCars = allNextCars\\destCars
          -- for each car it will be looked for other cars at the same position.
          -- returns a list like that: [[(pos1,_,(pos2,_)],[(pos3,_)],[(pos4,_)]...]
          -- the first position comprised two cars at it. 
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
          -- reworked the "two on one position" cars to correctness
          noTwoCarsOnPlace = map rightBeforeLeft extractDouble
          noDoubleCarsInside = withDoubleCars\\(concat extractDouble) 



-------------------------- < rightBeforeLeft > --------------------------------                      
-- returns the old position of the car who comes from the left, the other car
-- moves on. This will be used only, if two cars are on one cell.
rightBeforeLeft :: [(Pos,Cell)] -> [(Pos,Cell)]
rightBeforeLeft cars = 
    if (length cars)==2 
        -- both cars show at the right side.
        then whoIsTheRightOfTheCar firstCar secondCar :
             whoIsTheRightOfTheCar secondCar firstCar : []
        else error $"More ore less than two cars on one roadcell"
                    ++" in rightBeforeLeft!"
    where
          firstCar = cars!!0
          secondCar = cars!!1
 
-- this function looks for the car, which came from the left and rectified it.
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
          oldPosFirstCar = (\(Car _ _ _ old _) -> head $reverse old) $snd car1
          posCar1 = fst car1
          xCar1 = fst posCar1
          yCar1 = snd posCar1
          oldXCar1 = fst oldPosFirstCar
          oldYCar1 = snd oldPosFirstCar



-- looks on the right side of a car, if there is another car.
showRight :: Pos         -> 
             (Pos, Cell) -> 
             (Pos, Cell) -> 
             (Pos, Cell)
showRight rightPos car1 car2 =
    if rightPos==oldPosCar2
       then (returnOldPos car1, remLastPos car1)
       else car1
    
    where 
          oldPosCar2 = (\(Car _ _ _ old _) -> head $reverse old) $snd car2


-- returns the last position in oldPath
returnOldPos :: (Pos,Cell) -> Pos
returnOldPos (_,Car _ _ _ oldPath _) = head $reverse oldPath
returnOldPos _ = error "Must be a car cell in returnOldPos!"


-- it removes the last position from the oldPath and added the current position
-- to the path list
remLastPos :: (Pos,Cell) -> Cell
remLastPos (position,Car idC destC pathToGo oldPath col) =
    Car idC destC newPathToGo newOldPath col
    where newOldPath = reverse $ tail $ reverse oldPath
          newPathToGo = position:pathToGo
remLastPos _ = error "Must be a car cell in remLastPos!"



-------------------------- < nextCarsAndSignals > -----------------------------
-- It divides the signals and the cars
nextCarsAndSignals :: [[Cell]] -> [(Pos,Cell)] -> (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals staticC dynamicC (pos, cell) =
    case cell of 
         { (Signal _ _ _ _ _ _) -> signalStatus (pos, cell);
           (Car _ _ _ _ _)      -> checkSignal staticC dynamicC pos cell;
           (_)                  -> error $"Must be a signal or car cell in"
                                          ++" nextCarsAndSignals!"
         }


-- The signal status will be switched, if the time is over. 
-- If the time is not over, then the time will be decreased.
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


-------------------------- < checkSignal > ------------------------------------
-- Checks, if the next or the next but one roadpiece is a junction.
-- If thats true, the status of the signal will be checked.
-- If signal shows red, then the car will remain on the position, otherwise
-- it will move forward.
checkSignal ::  [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell ->  (Pos, Cell)
checkSignal stat dyn (x,y) cell = 
    -- looks at the next cell and if it's a junction, it will look for signals
    if length nextButOnePos > 1 
       -- checks, if there is a signal
       then if length nearestSignal > 0
               -- if signal shows green, the car moves on. Otherwise it
               -- remain on the current place
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



-- checks, if the destination is reached, if a car is on the next position,
-- cleares deadlocks at a junction and moves the car.
carStep :: [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell -> (Pos,Cell)
carStep staticL dyn (xa,ya) (Car idC (xd,yd) pathToGo pathOld col) = 
    -- Remove the car, if its on the destination or in the neighborhood.
    if (xa,ya) == (xd,yd) || 
       xa==xd && (ya==yd-1 || ya==yd+1) || 
       ya==yd && (xa==xd-1 || xa==xd+1)
       then
         --car is on his destination
         ((-1,-1), Car {ident=0, 
                        dest=(xd,yd), 
                        path=[], 
                        iWasThere=[], 
                        colour=col}
         )
       --if a car is on the next field
       else if length carOnNext > 0    
               -- checks, if a deadlock occures for 5 times
               -- looks, if the car has waited 5 times and if the otherNext
               -- field is not occupied. If that's true the car move
               -- the other way and the pathToGo will be cleared.
               then if oldCell == (xa,ya) && length carOnOtherNext <= 0
                
                       then (otherNext, Car idC 
                                            (xd,yd) 
                                            []
                                            (pathOld++[(xa,ya)])
                                            col)
               --else it will remain on the current place
                       else if null pathToGo
                            -- if pathToGo is empty, then find a path
                            then ((xa,ya), Car idC
                                               (xd,yd)
                                               (next:(path car))
                                               (pathOld++[(xa,ya)])
                                               col)
                            else ((xa,ya), Car idC
                                               (xd,yd)
                                               pathToGo
                                               (pathOld++[(xa,ya)])
                                               col)
            --otherwise it will move on
               else (next, car)     
    where 
          -- the next step for the car
          (next, car) = nextField staticL 
                          (getCell staticL (xa,ya)) 
                          (xa,ya) 
                          (Car idC (xd,yd) pathToGo pathOld col)
          -- the nextRoad list from the cell pointed by (xa,ya)
          nextRoadFromCell = nextRoad (getCell staticL (xa,ya))
          -- the next way, if first way is blocked and if it exists
          otherNext = if length nextRoadFromCell >1
                         then head $nextRoadFromCell\\[next]
                         else next
          -- filters a car, if there is one on the next position
          carOnNext = filter (\(pos,_) -> pos==next) dyn
          -- the same for the other way
          carOnOtherNext = filter (\(pos,_) -> pos==otherNext) dyn
          -- looks at the fourth position from behind of the old path list
          oldCell = if (length pathOld) > 4
                        then (reverse pathOld)!!4
                        else (-1,-1)
                            
carStep _ _ _ _ = error "Must be a car cell in carStep!"



-- uses the a-star algorithm if the next way list is empty and if
-- there is more than one way to go from current possition.
nextField :: [[Cell]] -> Cell -> Pos -> Cell -> (Pos, Cell)
nextField staticC 
          (Road _ _ next) 
          position 
          (Car idC destination pathToGo pathOld col) = 
    -- checks if it exists more then one next way
    if (length next > 1)
        -- checks if there exists a path to the destination
        then if null pathToGo
            -- if no path exists, then a-star will be used
                then if length pathAStar > 1 
                        then (head route, 
                              Car idC destination (tail route) newOldNext col)
                        else (head pathAStar, 
                              Car idC destination [] newOldNext col)
                else (head pathToGo,
                      Car idC destination (tail pathToGo) newOldNext col)
        -- only one next way exists and the car move this way
        else (head next, 
              Car idC
                  destination 
                  (pathToGo\\[head next]) 
                  newOldNext 
                  col
             )
    where pathAStar = aStar position destination staticC
          -- the path from a-star is from the destination to the current
          -- position and with the current position at the end.
          route = tail $reverse pathAStar
          -- remove (position) for the first time it 
          -- appears and put it at the end of the list!
          newOldNext = nub ((pathOld\\[position])++[position])
nextField _ _ _ _ = error "Must be a road cell in nextField!"


