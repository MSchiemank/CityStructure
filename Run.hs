module Run where
import Parse
import Data.List

{- The nextStep is first to generate a new dynamic list, with the moving cars on the next
   point in the City and the switching signals.Out of this a new city will be generated.-}
nextStep :: City -> City
nextStep city = City {width  = getCityWidth  city,
                      height = getCityHeight city,
                      static = getCityStatic city,
                      dynamic = allCarsWithoutTwoOnOneCell}
                where withDouble = filter (\(pos,cell) -> pos /= (-1,-1)) (map (nextCarsAndSignals (getCityStatic city) dyn) dyn) --build the next way of all cars 
                      dyn = getCityDynamic city
                      searchDoubleForEachElement = map (\(pos1,_) -> filter (\(pos,cell) -> pos == pos1) withDouble) withDouble 
                      extractDouble = nub $ filter (\x -> length x >1) searchDoubleForEachElement     --extract the double cars on one Cell
                      allCarsWithoutTwoOnOneCell = concat (map rightBeforeLeft extractDouble)++(withDouble\\(concat extractDouble)) --this will correct the two cars on one cell phenomenon


                      
-- returns the old position of the first car on the cell, the other car moves on
-- will be used, if two cars are on one cell.
rightBeforeLeft :: [(Pos,Cell)] -> [(Pos,Cell)]
rightBeforeLeft (x:xs) = (returnOldPos x, remLastPos x):xs



returnOldPos :: (Pos,Cell) -> Pos
returnOldPos (_,Car id dest oldPath) = head $reverse oldPath

remLastPos :: (Pos,Cell) -> Cell
remLastPos (_,Car id dest oldPath) = Car id dest $ reverse $ tail $ reverse oldPath





{- The cars look for the next field in the street cell. If there will be 2 next 
   fields, then it will look, which field is the nearest to the target. There for 
   are the wights. If the wight is equal, then the next but one fields will be
   looked for. And this fields have also a next field, then the wights for this will
   be created and the lower wight will elected the real next step.
   After the car has reached the destination, it will be removed from the city.-}
nextCarsAndSignals :: [[Cell]] -> [(Pos,Cell)] -> (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals static dynamic (pos, cell) = case cell of 
                                         { (Signal ident status stepToWait remainingSteps workWith against) -> signalStatus (pos, cell);
                                           (Car ident dest iWasThere)      -> checkSignal static dynamic pos cell}


-- switches the signals
signalStatus :: (Pos,Cell) -> (Pos,Cell)
signalStatus (pos, Signal id stat step remainingStep wW wA) =
                if remainingStep == 1
                   then (pos, Signal { ident         = id,
                                       status        = not stat,
                                       stepToWait    = step,
                                       remainingSteps= step,
                                       workWith      = wW,
                                       against       = wA})
                   else (pos, Signal { ident         = id,
                                       status        = stat,
                                       stepToWait    = step,
                                       remainingSteps= (remainingStep-1),
                                       workWith      = wW,
                                       against       = wA})


{- checks, if the next or the next but one roadpiece is a junction. If thats true, the status of the signal will be checked.
   If signal shows red, then the car will remain on the position, otherwise it will move forward.-}
checkSignal ::  [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell ->  (Pos, Cell)
checkSignal stat dyn (x,y) cell = if length nextButOnePos > 1 || length nextButTwoPos > 1
                                     then if length nearestSignal > 0
                                             then if and $ map (\(_,(Signal _ status _ _ _ _)) -> status) nearestSignal
                                                     then carStep stat dyn (x,y) cell
                                                     else ((x,y),cell)
                                             else carStep stat dyn (x,y) cell
                                     else carStep stat dyn (x,y) cell
                                  where nextPos = head((\(Road _ _ next) -> next) (getCell stat (x,y)))
                                        nextButOnePos = (\(Road _ _ next) -> next) (getCell stat nextPos)
                                        nextButTwoPos = map (\(Road _ _ next) -> next) $ map (getCell stat) nextButOnePos
                                        allSignals = filter (\(_,cell1) -> case cell1 of 
                                                               {(Signal ident status stepToWait remainingSteps workWith against) -> True;
                                                                 otherwise -> False}
                                                                 ) dyn
                                        nearestSignal = filter (\((x1,y1),cell) -> x1==x && (y1==y-1 || y1==y+1) ||
                                                                                   y1==y && (x1==x-1 || x1==x+1)
                                                                                   ) allSignals


getCell :: [[Cell]] -> (Pos) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)

-- Remove the car, if its on the destination or in the neighborhood.
carStep :: [[Cell]] -> [(Pos,Cell)] -> Pos -> Cell -> (Pos,Cell)
carStep static dyn (xa,ya) (Car id (xd,yd) pathOld) =
             if (xa,ya) == (xd,yd) || 
                xa==xd && (ya==yd-1 || ya==yd+1) || 
                ya==yd && (xa==xd-1 || xa==xd+1)
                then ((-1,-1), Car {ident=0, dest=(-1,-1), iWasThere=[] })
                else if length (filter (\(pos,cell) -> pos==next) dyn) > 0    --if a car is on the next field
                        then ((xa,ya), Car id (xd,yd) pathOld)                --then it will remain on the current place
                        else (next, Car id (xd,yd) (pathOld++[(xa,ya)]))      --otherwise it will move on
             where next = nextField static dyn (getCell static (xa,ya)) (xd,yd) pathOld (xa,ya)
           

-- Check the length of the next cell list.
nextField :: [[Cell]] -> [(Pos,Cell)] -> Cell -> Pos -> [Pos] -> Pos -> Pos
nextField static dyn (Road _ _ next) destination oldWay pos = 
                 if (length possibleWays) /=1
                    then findWay static destination (next\\oldWay) pos
                    else head possibleWays
                 where possibleWays = next\\oldWay                    


findWay :: [[Cell]] -> Pos -> [Pos] -> Pos -> Pos
findWay static destination list pos =
        findWayInCorrectDirection static destination (buildWeight destination list) pos
                     

-- builds the wight of the next cell
buildWeight :: Pos -> [Pos] -> [(Pos, Pos)]
buildWeight (xd,yd) list = map (\(x1,y1) -> (((if xd<x1 then x1-xd else xd-x1),(if yd<y1 then y1-yd else yd-y1)),(x1,y1))) list


{- This one decides which next cell will be used. If the weight from one
   of the way is lesser than the other, that will be the next weight.
    If it's equal, then the wight form each next but one cell decides what
    way will be taken.-}
findWayInCorrectDirection :: [[Cell]] -> Pos -> [(Pos, Pos)] -> Pos -> Pos
findWayInCorrectDirection _ _ [] _ = (-1,-1)
findWayInCorrectDirection static (xd,yd) (((wx1,wy1),(x1,y1)):((wx2,wy2),(x2,y2)):xs) (xa,ya)=
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
             nextCellNext1 = (\(Road _ _ next) -> next) (getCell static (x1,y1))
             nextWeight2 = minimum (map (\((x,y),_) -> x+y) weight2)
             weight2 = buildWeight (xd,yd) nextCellNext2
             nextCellNext2 = (\(Road _ _ next) -> next) (getCell static (x2,y2))
             


