module Main where 
--import Char
import Control.Concurrent
--import Data.List
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Events
import Graphics.UI.Gtk.Glade
import Parse
import Run

spaceCell :: Int
spaceCell = 10 

--Starting the programm with the gui
main :: IO()
main = do file <- readFile "city"--"signalTest"--

-- the city is now an IORef
          cityIO <- newIORef $parse $doInputString file
          c <- readIORef cityIO

          let winW = spaceCell * (getCityWidth c)
              winH = spaceCell * (getCityHeight c)


--set up the gui with relationships
          _ <- initGUI
          Just xml <- xmlNew "gui.glade"
          window   <- xmlGetWidget xml castToWindow "window1"
          reset <- xmlGetWidget xml castToButton "reset"
          start <- xmlGetWidget xml castToButton "start"
          stop <- xmlGetWidget xml castToButton "stop"
          step <- xmlGetWidget xml castToButton "stepButton"
          speedButton <- xmlGetWidget xml castToSpinButton "speed"
          grid <- xmlGetWidget xml castToCheckButton "gridButton"
          drawarea <- xmlGetWidget xml castToDrawingArea "drawingarea"

          _ <- on drawarea sizeRequest $ return (Requisition winW winH)

--settin the speed as an IORef var and the automation flag to False.
          speed <- spinButtonGetValueAsInt speedButton
          speedIO <- newIORef speed
          autoIO <- newIORef False

-- the events with the different handlings

-- resets the drawingarea to the beginnig
          _ <- onClicked reset $ do modifyIORef cityIO $do return $parse $doInputString file
                                    update grid drawarea cityIO 

-- starts the automatic by setting the autoIO flag
          _ <- onClicked start $ do modifyIORef autoIO $ do return True

-- stopps the automatic 
          _ <- onClicked stop $ do modifyIORef autoIO $ do return False

-- step by step the drawingarea will be changed          
          _ <- onClicked step $ do modifyIORef cityIO nextStep
                                   update grid drawarea cityIO  

-- the modifying of the speedIO var           
          _ <- onValueSpinned speedButton $do s2 <- spinButtonGetValueAsInt speedButton 
                                              modifyIORef speedIO (return s2)
                        
-- turns the gridd on and off
          _ <- onToggled grid $ update grid drawarea cityIO


-- for the first popup of the window 
          gridActive <- toggleButtonGetActive grid
          _ <- onExpose drawarea $exposeDraw drawarea gridActive cityIO


--starts a new IO thread for the automatic          
          _ <- forkIO (thread cityIO speedIO autoIO drawarea grid)


          _ <- onDestroy window mainQuit
          widgetShowAll window
          mainGUI



-----------------------------------------------------------------------------------------

-- the new thread for the automatic
thread :: IORef City -> IORef Int -> IORef Bool -> DrawingArea -> CheckButton -> IO ()
thread cityIO speedIO autoIO drawarea grid = do
    speed <- readIORef speedIO          --lift the IORef speedIO to int
    threadDelay (div (1000000) speed)      --need more time to look?
    auto <- readIORef autoIO            --like above
--    putStrLn "tick "
    if auto                            --automatic or not?
        then (do modifyIORef cityIO nextStep      --the next step in the game
                 widgetQueueDraw drawarea  )      -- new drawing of the drawarea
        else return ()


    thread cityIO speedIO autoIO drawarea grid --((\x -> if auto then (x+1) else x) i)       --Endless loop

-----------------------------------------------------------------------------------------

-- used when reset or step button are pushed. I don't like to lift the IO Monad
update :: CheckButton -> DrawingArea -> IORef City -> IO ()
update grid drawarea cityIO = do
    gridAct <- toggleButtonGetActive grid 
    (w,h) <- widgetGetSize drawarea
    drw <- widgetGetDrawWindow drawarea
    city <- readIORef cityIO
    renderWithDrawable drw $ cityDraw gridAct city w h




-- used only on expose event.
exposeDraw :: DrawingArea -> Bool -> IORef City -> Events.Event -> IO Bool
exposeDraw draw grid cityIO event = do
            (w,h) <- widgetGetSize draw
            drw <- widgetGetDrawWindow draw
            city <- readIORef cityIO
            renderWithDrawable drw $cityDraw grid city w h
            return (Events.eventSent event)
 

-----------------------------------------------------------------------------------------
--only the painting of the city.
cityDraw :: Bool -> City -> Int -> Int -> Render ()
cityDraw grid city w h = do 
    setSourceRGB 1 1 1                                  --background
    paint                                               --is painted

    if grid                                             --read the grid var. Turn grid on/off
       then drawGrid w h
       else return()

    mapM_ (drawStaticCell stat) arayPos               --draw the static of the city
    stroke
    mapM_ drawDynamicCell dyn                       --draw the dynamic of the city
    stroke
    where stat = getCityStatic city
          dyn = getCityDynamic city
          arayWidth = getCityWidth city
          arayHeight = getCityHeight city
          arayPos = [(x,y) | x <- [1..arayWidth], y <- [1..arayHeight]] --builds a little helping array



-- this draws the cars and the signals 
drawDynamicCell :: (Pos,Cell) -> Render ()
drawDynamicCell (pos,cell) = case cell of
    {(Parse.Signal _  stat _ _ _ _) -> (do
                             if stat
                                then setSourceRGB 0 1 0       -- for green colour
                                else setSourceRGB 1 0 0       -- for red colour
                             drawArcFilled pos);
     (Car _ _ _)      -> (do setSourceRGB 0 0 0       -- black is beautifull :)
                             drawArcFilled pos);
     (Road _ _ _)   -> error "No Road allowed in dynamic city list!";
     (Building _ _) -> error "No Building allowed in dynamic city list!";
     Empty          -> error "No Empty piece allowed in dynamic city list!"

    }



drawArcFilled :: Pos -> Render ()
drawArcFilled (x,y) = do
    let s = fromIntegral spaceCell
        xd = fromIntegral x
        yd = fromIntegral y
    setLineWidth (s*0.1)
    arc ((xd-0.5)*s) ((yd-0.5)*s) (0.3*s) 0 (2*pi)  --0.5 for the middle of the field and s is for the space of one piece
    fill



{- it draws all cell connections for the roads and the buildings
   for each static cell will be a connection to every cell, which are in the nextRoad list-}
drawStaticCell :: [[Cell]] -> Pos -> Render ()
drawStaticCell cellList pos = do
    let s = fromIntegral spaceCell
    setSourceRGB 0 0 0
    setLineWidth (s*0.1)
    case cell of
          { (Road _ _ nRoad)            -> mapM_ (drawStreetPart pos) nRoad;
            (Building _ _)              -> drawBuilding pos;
            Empty                       -> return ();
            (Car _ _ _)                 -> error "No cars allowed in static City list!";
            (Parse.Signal _ _ _ _ _ _)  -> error "No signals allowed in static City list!"}
    where cell = getCell cellList pos



-- this one draws a nice little building in the cell, very cool!
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


-- This draws a little connection from source to destination street part of a single street field
drawStreetPart :: Pos -> Pos -> Render ()
drawStreetPart (x1,y1) (x2,y2) = do
    let s = fromIntegral spaceCell
    moveTo (s*((fromIntegral x1)-0.5)) (s*((fromIntegral y1)-0.5))
    lineTo (s*((fromIntegral x2)-0.5)) (s*((fromIntegral y2)-0.5)) 



-- it paints the grid in the City for debugging or something else
drawGrid :: Int -> Int -> Render ()
drawGrid w h = do
  let colVal = 0.95
      s = fromIntegral spaceCell
  setSourceRGB colVal colVal colVal
  setLineWidth (s*0.05)
                  
  mapM_ (\x -> do moveTo (fromIntegral x) 0
                  lineTo (fromIntegral x) (fromIntegral h)
                  stroke)  [x1*spaceCell | x1 <- [0..(div w spaceCell)]]

  mapM_ (\y -> do moveTo 0 (fromIntegral y)
                  lineTo (fromIntegral w) (fromIntegral y)
                  stroke)  [y1*spaceCell | y1 <- [0..(div h spaceCell)]]



------------------------------------------------------------------------------------------
-- The next segment is the writing on the console and is not more used by the prog.
-- Only for debugging necessary or for the gog old times! :D
------------------------------------------------------------------------------------------
{-Builds an outputstring on the console. Each row of the lists will be printed as a
  single row. The preferred Cell is in the dynamic list of the city. If there is an empty
  field, the static list will be used. If there is a signal or a car, then this it will
  be printed. 
  -}



{-printCity :: City -> IO()
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
