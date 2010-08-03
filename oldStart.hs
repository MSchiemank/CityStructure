import System.Random (newStdGen)
import Data.IORef 
import Parse
import Run
import Control.Concurrent (threadDelay)

------------------------------------------------------------------------------------------
-- The next segment is the writing on the console and is not more used by the prog.
-- Only for debugging necessary or for the god old times! :D
------------------------------------------------------------------------------------------
{-Builds an outputstring on the console. Each row of the lists will be printed as a
  single row. The preferred Cell is in the dynamic list of the city. If there is an empty
  field, the static list will be used. If there is a signal or a car, then this it will
  be printed. 
  -}

test file = do --do file <- readFile "city"--"signalTest"--
--    parse genIO (doInputString file)
--    let i = 15--000
    gen <- newStdGen
    genIO <- newIORef gen
--    return ()
    run (parse genIO (doInputString file)) 1


run :: City -> Int -> IO()
run _ 0 = return ()
run city i = do 
    printCity city
    getLine --threadDelay 1000000--300000 getLine --
--    return ()
    run (nextStep city) i--(length (getCityDynamic city))--(length (getCityDynamic city))



printCity :: City -> IO()
printCity city = putStr (printStart city (getCityWidth city) (getCityHeight city) 1 1)


printStart :: City -> Int -> Int -> Int -> Int -> String
printStart city width height x y | x > width = ['\n']++printStart city width height 1 (y+1)
                                 | y > height = ['\n']
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



cellToChar :: Cell -> Pos -> Char
cellToChar cell pos = 
        case cell of
          { (Road ident name nextRoad)      -> if length nextRoad <=1
                                                  then  roadSign (head nextRoad) pos
                                                  else '\x271B';
            (Building ident name)           -> ' ';
            (Parse.Signal ident status stepToWait remainingSteps workWith against) -> signalSign status;
            (Car ident dest iWasThere _)      -> 'A';
            Empty                           -> ' '
          }


roadSign :: Pos -> Pos -> Char
roadSign (x1,y1) (x,y) | x1 < x = '\x2190'
                       | x1 > x = '\x2192'
                       | y1 < y = '\x2191'
                       | y1 > y = '\x2193'
                       

signalSign :: Bool -> Char
signalSign b = if b
                  then 'G'
                  else 'R'
