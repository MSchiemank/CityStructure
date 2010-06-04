import Data.List
import Char
import Control.Concurrent
import Parse

--Starting the programm with the main function eleven times
main :: IO()
main = do file <- readFile "city"
          let i = 12
          run (parse (doInputString file)) i


run :: City -> Int -> IO()
run _ 0 = return ()
run city i = do printCity city
                threadDelay 500000
                run (nextStep city) (i-1)



{- The nextStep is first to generate a new dynamic list, with the moving cars on the next
   point in the City and the switching signals.Out of this a new city will be generated.-}
nextStep :: City -> City
nextStep city = City {width  = getCityWidth  city,
                      height = getCityHeight city,
                      static = getCityStatic city,
                      dynamic = filter (\(pos,cell) -> pos /= (-1,-1)) (map (nextCarsAndSignals (getCityStatic city)) (getCityDynamic city))}






{- The cars look for the next field in the street cell. If there will be 2 next 
   fields, then it will look, which field is the nearest to the target. There for 
   are the wights. If the wight is equal, then the next but one fields will be
   looked for. And this fields have also a next field, then the wights for this will
   be created and the lower wight will elected the real next step.
   After the car has reached the destination, it will be removed from the city.-}
nextCarsAndSignals :: [[Cell]] -> (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals static (pos, cell) = case cell of 
                                         { (Signal ident workWith against) -> (pos, cell);
                                           (Car ident dest iWasThere)      -> carStep static pos cell}

-- Remove the car, if its on the destination or in the neighborhood.
carStep :: [[Cell]] -> Pos -> Cell -> (Pos,Cell)
carStep static (xa,ya) (Car id (xd,yd) pathOld) =
             if (xa,ya) == (xd,yd) || 
                xa==xd && (ya==yd-1 || ya==yd+1) || 
                ya==yd && (xa==xd-1 || xa==xd+1)
                then ((-1,-1), Car {ident=0, dest=(-1,-1), iWasThere=[] })
                else (nextField static (getCell static (xa,ya)) (xd,yd) pathOld (xa,ya), Car id (xd,yd) (pathOld++[(xa,ya)]))
           


-- Check the length of the next cell list.
nextField :: [[Cell]] -> Cell -> Pos -> [Pos] -> Pos -> Pos
nextField static (Road _ _ next) destination oldWay pos = 
                 if (length next) >1
                    then findWay static destination (next\\oldWay) pos
                    else head next
                    


findWay :: [[Cell]] -> Pos -> [Pos] -> Pos -> Pos
findWay static destination list pos =
        findWayInCorrectDirection static destination (buildWeight destination list) pos
                     

-- builds the wight of the next cell
buildWeight :: Pos -> [Pos] -> [(Int, Pos)]
buildWeight (xd,yd) list = map (\(x1,y1) -> ((if xd<x1 then x1-xd else xd-x1)+(if yd<y1 then y1-yd else yd-y1),(x1,y1))) list



{- This one decides which next cell will be used. If the weight from one
   of the way is lesser than the other, that will be the next weight.
    If it's equal, then the wight form each next but one cell decides what
    way will be taken.-}
findWayInCorrectDirection :: [[Cell]] -> Pos -> [(Int, Pos)] -> Pos -> Pos
findWayInCorrectDirection static (xd,yd) ((w1,(x1,y1)):(w2,(x2,y2)):xs) (xa,ya)=
    if w1 == w2
       then if (minimum nextCellList1) < (minimum nextCellList2)
             then (x1,y1)
             else (x2,y2)
       else (\(_,pos) -> pos) (minimum ((w1,(x1,y1)):(w2,(x2,y2)):xs))
    where nextCellList1 = buildWeight (xd,yd) ((\(Road _ _ next) -> next) (getCell static (x1,y1)))
          nextCellList2 = buildWeight (xd,yd) ((\(Road _ _ next) -> next) (getCell static (x2,y2)))
         


---------------------------------------------------------------
{-Builds an outputstring on the console. Each row of the lists will be printed as a
  single row. The preferred Cell is in the dynamic list of the city. If there is an empty
  field, the static list will be used. If there is a signal or a car, then this it will
  be printed. 
  -}
printCity :: City -> IO()
printCity city = putStr (printStart city (getCityWidth city) (getCityHeight city) 1 1)


printStart :: City -> Int -> Int -> Int -> Int -> String
printStart city width height x y | x > width = ['\n']++printStart city width height 1 (y+1)
                                 | y > height = []
                                 | otherwise = [printCell city (x,y)]
                                               ++printStart city width height (x+1) y

printCell :: City -> Pos -> Char
printCell city pos = if not(null dyn)
                        then cellToChar (getCellFromTuple (head dyn)) pos
                        else cellToChar stat pos
                       where dyn = filter (\(y,_) -> y==pos) (getCityDynamic city)
                             stat = getCell (getCityStatic city) pos
    
getCellFromTuple :: (Pos,Cell) -> Cell
getCellFromTuple (pos,cell) = cell


getCell :: [[Cell]] -> (Pos) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)


cellToChar :: Cell -> Pos -> Char
cellToChar cell pos = 
        case cell of
          { (Road ident name nextRoad)      -> if length nextRoad <=1
                                                  then  roadSign (head nextRoad) pos
                                                  else '\x271B';
            (Building ident name)           -> ' ';
            (Signal ident workWith against) -> ' ';
            (Car ident dest iWasThere)      -> 'C';
            Empty                           -> ' '
          }


roadSign :: Pos -> Pos -> Char
roadSign (x1,y1) (x,y) | x1 < x = '\x2190'
                       | x1 > x = '\x2192'
                       | y1 < y = '\x2191'
                       | y1 > y = '\x2193'
                       


