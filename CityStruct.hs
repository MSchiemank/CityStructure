module Main where 
import Char
import Control.Concurrent
import Data.List
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Events
import Graphics.UI.Gtk.Glade
import Parse
import Run


spaceCell = 10 ::Int

--Starting the programm with the gui
main :: IO()
main = do file <- readFile "city"--"signalTest"--

          cityIO <- newIORef $parse $doInputString file
          c <- readIORef cityIO

          let i = 15--000
              --city = parse (doInputString file)
              winW = spaceCell * (getCityWidth c)
              winH = spaceCell * (getCityHeight c)

          initGUI
          Just xml <- xmlNew "gui.glade"
          window   <- xmlGetWidget xml castToWindow "window1"
          reset <- xmlGetWidget xml castToButton "reset"
          start <- xmlGetWidget xml castToButton "start"
          stop <- xmlGetWidget xml castToButton "stop"
          step <- xmlGetWidget xml castToButton "stepButton"
          speedButton <- xmlGetWidget xml castToSpinButton "speed"
          grid <- xmlGetWidget xml castToCheckButton "gridButton"
          drawarea <- xmlGetWidget xml castToDrawingArea "drawingarea"

          drawarea `on` sizeRequest $ return (Requisition winW winH)

          speed <- spinButtonGetValueAsInt speedButton
          speedIO <- newIORef speed
          autoIO <- newIORef False

          onClicked reset $ do cityIO <- newIORef $parse $doInputString file
                               update grid drawarea cityIO 

          onClicked start $ do modifyIORef autoIO $ do return True

          onClicked stop $ do modifyIORef autoIO $ do return False
          
          onClicked step $ do city <- readIORef cityIO
                              modifyIORef cityIO nextStep
                              update grid drawarea cityIO  
            
          onValueSpinned speedButton $do s2 <- spinButtonGetValueAsInt speedButton 
                                         modifyIORef speedIO (return s2)
                        

          onToggled grid $ update grid drawarea cityIO
 
          gridActive <- toggleButtonGetActive grid
          onExpose drawarea $exposeDraw drawarea gridActive cityIO
          
          forkOS (thread cityIO speedIO autoIO drawarea grid)


          onDestroy window mainQuit
          widgetShowAll window
          mainGUI

--          run (parse (doInputString file)) i

thread :: IORef City -> IORef Int -> IORef Bool -> DrawingArea -> CheckButton -> IO ()
thread cityIO speedIO autoIO drawarea grid = do
    speed <- readIORef speedIO
    auto <- readIORef autoIO
    putStrLn "tick"
    threadDelay (div (10^6) speed)
    if auto
        then (do city <- readIORef cityIO
                 modifyIORef cityIO nextStep
                 update grid drawarea cityIO)
        else return ()

    widgetQueueDraw drawarea
    thread cityIO speedIO autoIO drawarea grid


{-run :: City -> Int -> IO()
run _ 4 = return ()
run city i = do --printCity city
                --threadDelay 1000000--300000 getLine --
                --run (nextStep city) (length (getCityDynamic city))--(i-1)--(length (getCityDynamic city))-}

update :: CheckButton -> DrawingArea -> IORef City -> IO ()
update grid drawarea cityIO = do
    gridAct <- toggleButtonGetActive grid 
    (w,h) <- widgetGetSize drawarea
    drw <- widgetGetDrawWindow drawarea
    city <- readIORef cityIO
    renderWithDrawable drw $ cityDraw gridAct city w h





exposeDraw :: DrawingArea -> Bool -> IORef City -> Events.Event -> IO Bool
exposeDraw draw grid cityIO event = do
            (w,h) <- widgetGetSize draw
            drw <- widgetGetDrawWindow draw
            city <- readIORef cityIO
            renderWithDrawable drw $cityDraw grid city w h
            return (Events.eventSent event)
 


cityDraw :: Bool -> City -> Int -> Int -> Render ()
cityDraw grid city w h = do 
    setSourceRGB 1 1 1
    paint

    if grid
       then drawGrid w h
       else return()
    mapM_ (drawStaticCell static) arayPos
    stroke
    mapM_ drawDynamicCell dynamic 
    stroke
    where static = getCityStatic city
          dynamic = getCityDynamic city
          arayWidth = getCityWidth city
          arayHeight = getCityHeight city
          arayPos = [(x,y) | x <- [1..arayWidth], y <- [1..arayHeight]]



{- this draws the cars and the signals-}
drawDynamicCell :: (Pos,Cell) -> Render ()
drawDynamicCell (pos,cell) = case cell of
    {(Parse.Signal ident 
                   status 
                   stepToWait
                   remainingSteps
                   workWith
                   against) -> (do if status
                                      then setSourceRGB 0 1 0
                                      else setSourceRGB 1 0 0
                                   drawArcFilled pos);
     (Car ident dest iWasThere)      -> do setSourceRGB 0 0 0 
                                           drawArcFilled pos
    }




drawArcFilled :: Pos -> Render ()
drawArcFilled (x,y) = do
    let s = fromIntegral spaceCell
        xd = fromIntegral x
        yd = fromIntegral y
    setLineWidth (s*0.1)
    arc ((xd-0.5)*s) ((yd-0.5)*s) (0.3*s) 0 (2*pi)
    fill



{- it draws all cell connections for the roads. And the other things like Buildings or
   signlas and cars were drawed too-}
drawStaticCell :: [[Cell]] -> Pos -> Render ()
drawStaticCell cellList pos = do
    let s = fromIntegral spaceCell
    setSourceRGB 0 0 0
    setLineWidth (s*0.1)
    case cell of
          { (Road ident name nextRoad)      -> mapM_ (drawStreetPart pos) nextRoad;
            (Building ident name)           -> drawBuilding pos;
            Empty                           -> return ()}
    where cell = getCell cellList pos



{- this one draws a nice little building in the cell, very cool!-}
drawBuilding :: Pos -> Render ()
drawBuilding (x,y) = do
    let s = fromIntegral spaceCell
        xd = fromIntegral x
        yd = fromIntegral y
    setSourceRGB 0 0 0
    setLineWidth (s*0.1)
    moveTo ((xd-0.2)*s) ((yd-0.1)*s)
    lineTo ((xd-0.8)*s) ((yd-0.1)*s)
    lineTo ((xd-0.8)*s) ((yd-0.5)*s)
    lineTo ((xd-0.5)*s) ((yd-0.8)*s)
    lineTo ((xd-0.2)*s) ((yd-0.5)*s)
    lineTo ((xd-0.2)*s) ((yd-0.05)*s)


-- This draws a street part of a single street field
drawStreetPart :: Pos -> Pos -> Render ()
drawStreetPart (x1,y1) (x2,y2) = do
    let s = fromIntegral spaceCell
    moveTo (s*((fromIntegral x1)-0.5)) (s*((fromIntegral y1)-0.5))
    lineTo (s*((fromIntegral x2)-0.5)) (s*((fromIntegral y2)-0.5)) 



{- it paints the grid in the City for debugging-}
drawGrid :: Int -> Int -> Render ()
drawGrid w h = do
  let colVal = 0.95
      s = fromIntegral spaceCell
  setSourceRGB colVal colVal colVal
  setLineWidth (s*0.05)
                  
  mapM_ (\x -> do moveTo (fromIntegral x) (fromIntegral 0)
                  lineTo (fromIntegral x) (fromIntegral h)
                  stroke)  [x1*spaceCell | x1 <- [0..(div w spaceCell)]]

  mapM_ (\y -> do moveTo (fromIntegral 0) (fromIntegral y)
                  lineTo (fromIntegral w) (fromIntegral y)
                  stroke)  [y1*spaceCell | y1 <- [0..(div h spaceCell)]]

---------------------------------------------------------------
{-Builds an outputstring on the console. Each row of the lists will be printed as a
  single row. The preferred Cell is in the dynamic list of the city. If there is an empty
  field, the static list will be used. If there is a signal or a car, then this it will
  be printed. 
  -}


{-it was used before the GUI was programmed
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
            (Car ident dest iWasThere)      -> 'A';
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
                  else 'R'-}
