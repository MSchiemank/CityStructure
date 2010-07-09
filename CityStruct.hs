module Main where 
import Control.Concurrent
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Events
import Graphics.UI.Gtk.Glade
import Parse
import Run
import System.Random


spaceCell :: Int
spaceCell = 7

--Starting the programm with the gui
main :: IO()
main = do file <- readFile "city"--"rightTest"--"signalTest"--

-- the city is now an IORef
          gen <- newStdGen
          genIO <- newIORef gen
          cityIO <- newIORef $parse genIO $doInputString file
          c <- readIORef cityIO

          let fieldWidth = getCityWidth c
              fieldHeight = getCityHeight c
              winW = if 380< spaceCell * fieldWidth
                        then spaceCell * fieldWidth
                        else 380
              winH = if 150< spaceCell * fieldHeight
                        then spaceCell * fieldHeight
                        else 150

--set up the gui with relationships
          _ <- initGUI
          Just xml <- xmlNew "gui.glade"
          window   <- xmlGetWidget xml castToWindow "window1"
          reset <- xmlGetWidget xml castToToolButton "reset"
          start <- xmlGetWidget xml castToToolButton "start"
          stop <- xmlGetWidget xml castToToolButton "stop"
          step <- xmlGetWidget xml castToToolButton "stepButton"
          speedButton <- xmlGetWidget xml castToSpinButton "speed"
          grid <- xmlGetWidget xml castToCheckButton "gridButton"
          drawarea <- xmlGetWidget xml castToDrawingArea "drawingarea"

          _ <- on drawarea sizeRequest $ return (Requisition winW winH)

-- setting the speed as an IORef var and the automation flag to False.
          speed <- spinButtonGetValueAsInt speedButton
          speedIO <- newIORef speed
          autoIO <- newIORef False


-- setting the sensitivity of the whole buttons
          mapM_ (flip widgetSetSensitivity False) [reset,stop]
          mapM_ (flip widgetSetSensitivity True) [start,step]

-- the events with the different handlings

-- resets the drawingarea to the beginnig
          _ <- onToolButtonClicked reset $ do 
                modifyIORef cityIO $do return $parse genIO $doInputString file
                modifyIORef autoIO $do return False
                mapM_ (flip widgetSetSensitivity False) [reset,stop]
                mapM_ (flip widgetSetSensitivity True) [start,step]
                update grid drawarea cityIO fieldWidth fieldHeight

-- starts the automatic by setting the autoIO flag
          _ <- onToolButtonClicked start $ do
                modifyIORef autoIO $ do return True
                mapM_ (flip widgetSetSensitivity False) [reset,start,step]
                mapM_ (flip widgetSetSensitivity True) [stop]

-- stopps the automatic 
          _ <- onToolButtonClicked stop $ do
                modifyIORef autoIO $ do return False
                mapM_ (flip widgetSetSensitivity False) [stop]
                mapM_ (flip widgetSetSensitivity True) [reset,start,step]


-- step by step the drawingarea will be changed          
          _ <- onToolButtonClicked step $ do 
                modifyIORef cityIO nextStep
                modifyIORef autoIO $ do return False
                mapM_ (flip widgetSetSensitivity True) [reset]
                update grid drawarea cityIO fieldWidth fieldHeight

-- the modifying of the speedIO var           
          _ <- onValueSpinned speedButton $do s2 <- spinButtonGetValueAsInt speedButton 
                                              modifyIORef speedIO (return s2)
                        
-- turns the gridd on and off
          _ <- onToggled grid $ update grid drawarea cityIO fieldWidth fieldHeight


-- for the first popup of the window 
          gridActive <- toggleButtonGetActive grid
          _ <- onExpose drawarea $exposeDraw drawarea gridActive cityIO fieldWidth fieldHeight


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

-- used when reset or step button are pushed. 
update :: CheckButton -> DrawingArea -> IORef City -> Int -> Int -> IO ()
update grid drawarea cityIO fieldW fieldH = do
    gridAct <- toggleButtonGetActive grid 
    (w,h) <- widgetGetSize drawarea
    drw <- widgetGetDrawWindow drawarea
    city <- readIORef cityIO
    let space = minimum [div w fieldW, div h fieldH]
    renderWithDrawable drw $ cityDraw gridAct city space w h




-- used only on expose event.
exposeDraw :: DrawingArea -> Bool -> IORef City -> Int -> Int ->  Events.Event -> IO Bool
exposeDraw draw grid cityIO fieldW fieldH event = do
            (w,h) <- widgetGetSize draw
            drw <- widgetGetDrawWindow draw
            city <- readIORef cityIO
            let space = minimum [div w fieldW, div h fieldH]
            renderWithDrawable drw $cityDraw grid city space w h
            return (Events.eventSent event)
 

-----------------------------------------------------------------------------------------
--only the painting of the city.
cityDraw :: Bool -> City -> Int -> Int -> Int -> Render ()
cityDraw grid city space w h = do 
    setSourceRGB 1 1 1                                  --background
    paint                                               --is painted

    if grid                                             --read the grid var. Turn grid on/off
       then drawGrid space w h
       else return()

    mapM_ (drawStaticCell stat space) arayPos         --draw the static of the city
    stroke
    mapM_ (drawDynamicCell stat space) dyn            --draw the dynamic of the city
    stroke
    where 
          stat = getCityStatic city
          dyn = getCityDynamic city
          arayWidth = getCityWidth city
          arayHeight = getCityHeight city
          arayPos = [(x,y) | x <- [1..arayWidth], y <- [1..arayHeight]] --builds a little helping array



-- this draws the cars and the signals 
drawDynamicCell :: [[Cell]] -> Int -> (Pos,Cell) -> Render ()
drawDynamicCell staticC space (pos,cell) = case cell of
    {(Parse.Signal _  stat _ _ _ _) -> (do
                             if stat
                                then setSourceRGB 0 1 0       -- for green colour
                                else setSourceRGB 1 0 0       -- for red colour
                             drawArcFilled pos space);
     (Car _ _ oldPath (r, g, b))-> (do 
            setSourceRGB r g b       -- draw the cars with their colours
            drawTriangleFilled pos oldPath space $getCell staticC pos);
     (Road _ _ _)   -> error "No Road allowed in dynamic city list!";
     (Building _ _) -> error "No Building allowed in dynamic city list!";
     Empty          -> error "No Empty piece allowed in dynamic city list!"

    }



drawArcFilled :: Pos -> Int -> Render ()
drawArcFilled (x,y) space = do
    let s = fromIntegral space
        xd = fromIntegral x
        yd = fromIntegral y
--    setLineWidth (s*0.3)
    arc ((xd-0.5)*s) ((yd-0.5)*s) (0.3*s) 0 (2*pi) --0.5 for the middle of the field and s is for the space of one piece
    fill

drawTriangleFilled :: Pos -> [Pos] -> Int -> Cell -> Render ()
drawTriangleFilled (x,y) oldPos space staticC = do
    let s = fromIntegral space
        xd = fromIntegral x
        yd = fromIntegral y
    setLineWidth (s*0.3)
    if null oldPos
       then if nextY < y
               then drawTriangleUp (xd,yd) s
               else if nextX<x 
                       then drawTriangleLeft (xd,yd) s
                       else if nextY > y 
                               then drawTriangleDown (xd,yd) s
                               else drawTriangleRight (xd,yd) s
       else if oldY < y
               then drawTriangleDown (xd,yd) s
               else if oldX<x 
                       then drawTriangleRight (xd,yd) s
                       else if oldY > y 
                               then drawTriangleUp (xd,yd) s
                               else drawTriangleLeft (xd,yd) s
                                           
    fill
    where oldX = fst $ head $ reverse oldPos
          oldY = snd $ head $ reverse oldPos
          nextX = fst $ head $ (\(Road _ _ nextR) -> nextR) staticC
          nextY = snd $ head $ (\(Road _ _ nextR) -> nextR) staticC


-- Next four functions will draw a triangle in the known direction

drawTriangleUp:: (Double, Double) -> Double -> Render ()
drawTriangleUp (x,y) s = do
    moveTo ((x-0.5)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.1)*s)
    lineTo ((x-0.9)*s) ((y-0.1)*s)
    closePath

drawTriangleDown:: (Double, Double) -> Double -> Render ()
drawTriangleDown (x,y) s = do
    moveTo ((x-0.9)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.9)*s)
    lineTo ((x-0.5)*s) ((y-0.1)*s)
    closePath


drawTriangleLeft:: (Double, Double) -> Double -> Render ()
drawTriangleLeft (x,y) s = do
    moveTo ((x-0.1)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.1)*s)
    lineTo ((x-0.9)*s) ((y-0.5)*s)
    closePath


drawTriangleRight:: (Double, Double) -> Double -> Render ()
drawTriangleRight (x,y) s = do
    moveTo ((x-0.9)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.5)*s)
    lineTo ((x-0.9)*s) ((y-0.1)*s)
    closePath


{- it draws all cell connections for the roads and the buildings
   for each static cell will be a connection to every cell, which are in the nextRoad list-}
drawStaticCell :: [[Cell]] -> Int -> Pos -> Render ()
drawStaticCell cellList space pos = do
    let s = fromIntegral space
    setSourceRGB 0 0 0
    setLineWidth (s*0.1)
    case cell of
          { (Road _ _ nRoad)            -> mapM_ (drawStreetPart space pos) nRoad;
            (Building _ _)              -> drawBuilding space pos;
            Empty                       -> return ();
            (Car _ _ _ _)               -> error "No cars allowed in static City list!";
            (Parse.Signal _ _ _ _ _ _)  -> error "No signals allowed in static City list!"}
    where cell = getCell cellList pos



-- this one draws a nice little building in the cell, very cool!
drawBuilding :: Int -> Pos -> Render ()
drawBuilding space (x,y) = do
    let s = fromIntegral space
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
drawStreetPart :: Int -> Pos -> Pos -> Render ()
drawStreetPart space (x1,y1) (x2,y2) = do
    let s = fromIntegral space
    moveTo (s*((fromIntegral x1)-0.5)) (s*((fromIntegral y1)-0.5))
    lineTo (s*((fromIntegral x2)-0.5)) (s*((fromIntegral y2)-0.5)) 



-- it paints the grid in the City for debugging or something else
drawGrid :: Int -> Int -> Int -> Render ()
drawGrid space w h = do
  let colVal = 0.90
      s = fromIntegral space

  setSourceRGB colVal colVal colVal
  setLineWidth (s*0.05)

-- vertical lines                  
  mapM_ (\x -> do moveTo (fromIntegral x) 0
                  lineTo (fromIntegral x) (fromIntegral h)
                  stroke)  [x1*space | x1 <- [0..(div w space)]]

-- horizontal lines
  mapM_ (\y -> do moveTo 0 (fromIntegral y)
                  lineTo (fromIntegral w) (fromIntegral y)
                  stroke)  [y1*space | y1 <- [0..(div h space)]]



------------------------------------------------------------------------------------------
-- The next segment is the writing on the console and is not more used by the prog.
-- Only for debugging necessary or for the god old times! :D
------------------------------------------------------------------------------------------
{-Builds an outputstring on the console. Each row of the lists will be printed as a
  single row. The preferred Cell is in the dynamic list of the city. If there is an empty
  field, the static list will be used. If there is a signal or a car, then this it will
  be printed. 
  -}

{-test genIO file= --do file <- readFile "city"--"signalTest"--
          parse genIO (doInputString file)
--          let i = 15--000
--          gen <- newStdGen
--          genIO <- newIORef gen
--          return ()
--          run (parse genIO (doInputString file)) i


run :: City -> Int -> IO()
run _ 4 = return ()
run city i = do 
--                printCity city
--                threadDelay 1000000--300000 getLine --
                return ()
                --run (nextStep city) (length (getCityDynamic city))--(i-1)--(length (getCityDynamic city))



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
                  else 'R'-}
