import Data.List
import Char
import Control.Concurrent
import Parse

--Starting the programm with the main function eleven times
main :: IO()
main = do file <- readFile "city"
          let i = 10
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
                      dynamic = filter (\(pos,cell) -> pos /= (-1,-1)) (map (nextCarsAndSignals) (getCityDynamic city))}


{- The cars move along the path. The next element of the path will be the next position.
   After the car has reached the destination, it will be removed from the city.-}
nextCarsAndSignals :: (Pos,Cell) -> (Pos,Cell)
nextCarsAndSignals (pos, cell) = case cell of 
                                    { (Signal ident workWith against) -> (pos, cell);
                                      (Car ident path)                -> carStep (pos, cell)}

carStep :: (Pos,Cell) -> (Pos,Cell)
carStep (pos,(Car id pathOld)) = if (null pathOld)
                                    then ((-1,-1), Car {ident=0, path=[]})
                                    else (head pathOld,Car {ident = id, path = tail(pathOld)})


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


getCell :: [[Cell]] -> (Int,Int) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)


cellToChar :: Cell -> Pos -> Char
cellToChar cell pos = 
        case cell of
          { (Road ident name nextRoad)      -> if length nextRoad <=1
                                                  then  roadSign (head nextRoad) pos
                                                  else '\x271B';
            (Building ident name)           -> ' ';
            (Signal ident workWith against) -> ' ';
            (Car ident path)                -> 'C';
            Empty                           -> ' '
          }


roadSign :: Pos -> Pos -> Char
roadSign (x1,y1) (x,y) | x1 < x = '\x2190'
                       | x1 > x = '\x2192'
                       | y1 < y = '\x2191'
                       | y1 > y = '\x2193'
                       


