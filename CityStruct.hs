module Main where 
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef 
import Data.List (nub, (\\))
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Events 
    (eventSent, Event)
import Graphics.UI.Gtk.Glade
import Parse
import Run
import System.Random (newStdGen)

----------------------------- global variable -----------------------
emptyCity :: City
emptyCity = City 0 0 [] []

minWidth :: Int
minWidth = 440

minHeight :: Int
minHeight = 150

spaceCell :: Int
spaceCell = 8


---------------------------- < main > -------------------------------
--Starting the programm with the gui
main :: IO()
main = do 
------------------------------------
--set up the gui with relationships
          _ <- initGUI
          xmlM <- xmlNew "gui.glade"
          let xml = case xmlM of
                 (Just caseXml) -> caseXml
                 Nothing        -> error $ "can't find the glade file"
                                           ++" \"gui.glade\""
                                           ++ "in the current directory"
          mainWindow   <- xmlGetWidget xml castToWindow "window1"
          open <- xmlGetWidget xml castToToolButton "open"
          saveButton <- xmlGetWidget xml castToToolButton "save"
          randomButton <- xmlGetWidget xml castToToolButton "randomCity"
          reset <- xmlGetWidget xml castToToolButton "reset"
          start <- xmlGetWidget xml castToToolButton "start"
          stop <- xmlGetWidget xml castToToolButton "stop"
          step <- xmlGetWidget xml castToToolButton "stepButton"
          speedButton <- xmlGetWidget xml castToSpinButton "speed"
          grid <- xmlGetWidget xml castToCheckButton "gridButton"
          drawarea <- xmlGetWidget xml castToDrawingArea "drawingarea"


------------------------------------
-- initialising the IORef variables

-- generates a standart generator for random numbers and build an IORef var
          gen <- newStdGen
          genIO <- newIORef gen

-- initialised the file variable as IORefs
          fileOpenPathIO <- newIORef ""
          fileSavePathIO <- newIORef ""
          fileIO <- newIORef ""
-- the city is now an empty City in IORef
          cityIO <- newIORef emptyCity 


-- setting the speed as an IORef var and the automation flag to False.
          speed   <- spinButtonGetValueAsInt speedButton
          speedIO <- newIORef speed
          autoIO  <- newIORef False


------------------------------------
-- setting the sensitivity of the whole buttons and the activity
-- of the grid button
          mapM_ (flip widgetSetSensitivity False)
            [reset,stop,start,step,saveButton]
          mapM_ (flip widgetSetSensitivity True) 
            [open,randomButton]
          toggleButtonSetActive grid True 


------------------------------------
-- the events with the different handlings


-- The onSizeRequest event:
-- This event will appear, if the drawarea is requesting it's size
          _ <- on drawarea sizeRequest $do
                (w, h) <- dynSize cityIO
                return (Requisition w h)


-- The openFileDialog:
-- If one file is selected, this file will be read, the city will be created
-- as an IORef City var, the drawingarea will be resized, the buttons will be
-- activated and the drawingarea will be painted.
          _ <- onToolButtonClicked open $do 
                    openFileDialog mainWindow fileOpenPathIO
                    filePath <- readIORef fileOpenPathIO
                    if null filePath
                       then return ()
                       else (do 
                                -- the contents of the file will be read
                                file <- readFile filePath
                                -- the file path to IORef 
                                modifyIORef fileIO (\_ -> file)
                                -- city will be generated and stored in IORef
                                modifyIORef cityIO 
                                    (\_ -> parse genIO $doInputString file)
                                -- a new title for the mainWindow
                                windowSetTitle mainWindow 
                                    $"City Structure - "++filePath
                                -- widget will be resized 
                                (w, h) <- dynSize cityIO
                                widgetSetSizeRequest drawarea w h
                                -- buttons will be activated
                                mapM_ (flip widgetSetSensitivity True) 
                                    [start,step,saveButton]
                                -- city will be painted
                                widgetQueueDraw drawarea)--update grid drawarea cityIO)





-- The saveFileDialog
          _ <- onToolButtonClicked saveButton $do
                    saveFileDialog mainWindow fileSavePathIO
                    fileSavePath <- readIORef fileSavePathIO
                    if null fileSavePath
                       then return ()
                       else (do
                                city <- readIORef cityIO
                                let string = cityToString city
                                writeFile fileSavePath string)


-- The random dialoge
          _ <- onToolButtonClicked randomButton
             $ do randValIO <- newIORef [(-1)::Int]
                  showRandDialog xml randValIO
                  randVal <- readIORef randValIO
                  windowSetTitle mainWindow $"City Structure - Random City"
                  if randVal /= [(-1)]
                   then do 
                           let 
                               input = doInputString rand
                               rand = randomCity randVal genIO
                           modifyIORef fileIO
                               (\_ -> rand)
                           modifyIORef cityIO 
                               (\_ -> parse genIO input)
                           (w, h) <- dynSize cityIO
                           widgetSetSizeRequest drawarea w h
                           mapM_ (flip widgetSetSensitivity True) 
                               [start,step,saveButton]
                           widgetQueueDraw drawarea
                   else return ()
                      




-- resets the drawingarea to the beginnig
          _ <- onToolButtonClicked reset $ do
                file <- readIORef fileIO
                let city = if null file
                              then emptyCity
                              else parse genIO $doInputString file
                modifyIORef cityIO (\_ -> city)
                modifyIORef autoIO (\_ -> False)
                mapM_ (flip widgetSetSensitivity False) [reset,stop]
                mapM_ (flip widgetSetSensitivity True) [start,step]
                widgetQueueDraw drawarea

-- starts the automatic by setting the autoIO flag
          _ <- onToolButtonClicked start $ do
                modifyIORef autoIO $ do return True
                mapM_ (flip widgetSetSensitivity False) 
                    [open,saveButton,randomButton,reset,start,step]
                mapM_ (flip widgetSetSensitivity True) [stop]

-- stopps the automatic 
          _ <- onToolButtonClicked stop $ do
                modifyIORef autoIO $ do return False
                mapM_ (flip widgetSetSensitivity False) [stop]
                mapM_ (flip widgetSetSensitivity True)
                    [open,saveButton,randomButton,reset,start,step]


-- step by step the drawingarea will be changed          
          _ <- onToolButtonClicked step $ do 
                modifyIORef cityIO nextStep
                modifyIORef autoIO $ do return False
                mapM_ (flip widgetSetSensitivity True) [reset]
                widgetQueueDraw drawarea


-- the modifying of the speedIO var           
          _ <- onValueSpinned speedButton $do
                s2 <- spinButtonGetValueAsInt speedButton 
                modifyIORef speedIO (return s2)
                        
-- turns the gridd on and off
          _ <- onToggled grid $ widgetQueueDraw drawarea


-- for the first popup of the mainWindow 
          _ <- onExpose drawarea $exposeDraw drawarea grid cityIO


--starts a new IO thread for the automatic          
          _ <- forkIO (thread cityIO speedIO autoIO drawarea)


          _ <- onDestroy mainWindow mainQuit
          widgetShowAll mainWindow
          mainGUI



---------------------------------------------------------------------

-- the new thread for the automatic
thread :: IORef City  ->
          IORef Int   -> 
          IORef Bool  -> 
          DrawingArea -> 
          IO ()
thread cityIO speedIO autoIO drawarea = do
    --lift the IORef speedIO to int
    speed <- readIORef speedIO   
    --need more time to look?
    threadDelay (div (1000000) speed)
    --like above
    auto <- readIORef autoIO            
--    putStrLn "tick "

    --automatic or not?
    if auto
        then (do modifyIORef cityIO nextStep --the next step in the game
                 widgetQueueDraw drawarea)   -- new drawing of the drawarea
        else return ()


    thread cityIO speedIO autoIO drawarea    --Endless loop

---------------------------------------------------------------------

-- used only on expose event.
exposeDraw :: DrawingArea -> 
              CheckButton -> 
              IORef City -> 
              Events.Event -> 
              IO Bool
exposeDraw draw grid cityIO event = do
            gridAct <- toggleButtonGetActive grid
            (w,h) <- widgetGetSize draw
            drw <- widgetGetDrawWindow draw
            city <- readIORef cityIO
--            (fieldW,fieldH) <- size
            let fieldW = getCityWidth city
                fieldH = getCityHeight city
                calcSpace = minimum [div w fieldW, div h fieldH]
                space = if (fieldW > 0 && calcSpace>spaceCell)
                           then calcSpace
                           else spaceCell
            renderWithDrawable drw $cityDraw gridAct city space w h
            return (Events.eventSent event)
 

---------------------------------------------------------------------
--only the painting of the city.
cityDraw :: Bool -> City -> Int -> Int -> Int -> Render ()
cityDraw grid city space w h = do
    --background sets colour to white
    setSourceRGB 1 1 1 
    --paints the background
    paint                                               

    --read the grid var. Turn grid on/off (True/False)
    if grid                                             
       then drawGrid space w h
       else return()

    --draw the statics of the city
    mapM_ (drawStaticCell stat space) arayPos         
    --draw the dynamics of the city
    mapM_ (drawDynamicCell stat space) dyn            

    where 
          stat = getCityStatic city
          dyn = getCityDynamic city
          arayWidth = getCityWidth city
          arayHeight = getCityHeight city
          --builds a little helping array
          arayPos = [(x,y) | x <- [1..arayWidth], y <- [1..arayHeight]] 



-- this draws the cars and the signals 
drawDynamicCell :: [[Cell]] -> Int -> (Pos,Cell) -> Render ()
drawDynamicCell staticC space (pos,cell) = case cell of
    {(Parse.Signal _  stat _ _ _ _) -> (do
                             if stat
                                then setSourceRGB 0 1 0   -- for green colour
                                else setSourceRGB 1 0 0   -- for red colour
                             drawArcFilled pos space 0.3);
     (Car _ (x,y) oldPath (r, g, b))-> (do 
            setSourceRGB r g b       -- draw the cars with their colours
            let point = filterHouseAt staticC (x,y)
            if pos == (-1,-1)
               then drawBuilding space point
               else do
                drawTriangleFilled pos oldPath space $getCell staticC pos
                drawArcFilled point space 0.2);
     (Road _ _ _)   -> error "No Road allowed in dynamic city list!";
     (Building _ _) -> error "No Building allowed in dynamic city list!";
     Empty          -> error "No Empty piece allowed in dynamic city list!"

    }



drawArcFilled :: Pos -> Int -> Double -> Render ()
drawArcFilled (x,y) space radius = do
    let s = fromIntegral space
        xd = fromIntegral x
        yd = fromIntegral y

    --0.5 for the middle point of the field and s is for the space of one cell
    if radius == 0.2
        then arc ((xd-0.5)*s) ((yd-0.3)*s) (radius*s) 0 (2*pi) 
        else arc ((xd-0.5)*s) ((yd-0.5)*s) (radius*s) 0 (2*pi)
    fill
    stroke

drawTriangleFilled :: Pos -> [Pos] -> Int -> Cell -> Render ()
drawTriangleFilled (x,y) oldPos space staticC = do
    let s = fromIntegral space
        xd = fromIntegral x
        yd = fromIntegral y
    setLineWidth (s*0.3)
    if null oldPosWithoutActualPos
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
    
    where oldPosWithoutActualPos = (nub oldPos)\\[(x,y)]
          oldX = fst $ head $ reverse $(nub oldPos)\\[(x,y)]
          oldY = snd $ head $ reverse $(nub oldPos)\\[(x,y)]
          nextX = fst $ head $ (\(Road _ _ nextR) -> nextR) staticC
          nextY = snd $ head $ (\(Road _ _ nextR) -> nextR) staticC


-- Next four functions will draw a triangle in the known direction

drawTriangleUp:: (Double, Double) -> Double -> Render ()
drawTriangleUp (x,y) s = do
    moveTo ((x-0.5)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.1)*s)
    lineTo ((x-0.9)*s) ((y-0.1)*s)
    closePath
    fill
    stroke

drawTriangleDown:: (Double, Double) -> Double -> Render ()
drawTriangleDown (x,y) s = do
    moveTo ((x-0.9)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.9)*s)
    lineTo ((x-0.5)*s) ((y-0.1)*s)
    closePath
    fill
    stroke


drawTriangleLeft:: (Double, Double) -> Double -> Render ()
drawTriangleLeft (x,y) s = do
    moveTo ((x-0.1)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.1)*s)
    lineTo ((x-0.9)*s) ((y-0.5)*s)
    closePath
    fill
    stroke


drawTriangleRight:: (Double, Double) -> Double -> Render ()
drawTriangleRight (x,y) s = do
    moveTo ((x-0.9)*s) ((y-0.9)*s)
    lineTo ((x-0.1)*s) ((y-0.5)*s)
    lineTo ((x-0.9)*s) ((y-0.1)*s)
    closePath
    fill
    stroke


-- it draws all cell connections for the roads and the buildings
-- for each static cell will be a connection to every cell, which are in the 
-- nextRoad list
drawStaticCell :: [[Cell]] -> Int -> Pos -> Render ()
drawStaticCell cellList space pos = do
    setSourceRGB 0 0 0
    case cell of
          { (Road _ _ nRoad)            -> mapM_ (drawStreetPart space pos)
                                                 nRoad;
            (Building _ _)              -> drawBuilding space pos;
            Empty                       -> return ();
            (Car _ _ _ _)               -> error $"No cars allowed in"
                                                  ++" static City list!";
            (Parse.Signal _ _ _ _ _ _)  -> error $"No signals allowed in"
                                                  ++" static City list!"}
    where cell = getCell cellList pos



-- this one draws a nice little building in the cell, very cool!
drawBuilding :: Int -> Pos -> Render ()
drawBuilding space (x,y) = do
    let s = fromIntegral space
        xd = fromIntegral x
        yd = fromIntegral y
--    setSourceRGB 0 0 0
    setLineWidth (s*0.05)
    moveTo ((xd-0.2)*s) ((yd-0.1)*s)
    lineTo ((xd-0.8)*s) ((yd-0.1)*s)
    lineTo ((xd-0.8)*s) ((yd-0.5)*s)
    lineTo ((xd-0.2)*s) ((yd-0.5)*s)
    closePath
    stroke

    moveTo ((xd-0.9)*s) ((yd-0.5)*s)
    lineTo ((xd-0.5)*s) ((yd-0.8)*s)
    lineTo ((xd-0.1)*s) ((yd-0.5)*s)
    stroke


-- This draws a little connection from source to destination street part of a
-- single street field
drawStreetPart :: Int -> Pos -> Pos -> Render ()
drawStreetPart space (x1,y1) (x2,y2) = do
    let s = fromIntegral space
    setLineWidth (s*0.1)
    moveTo (s*((fromIntegral x1)-0.5)) (s*((fromIntegral y1)-0.5))
    lineTo (s*((fromIntegral x2)-0.5)) (s*((fromIntegral y2)-0.5)) 
    stroke



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

-------------------------------- < openFileDialog > ---------------------------
openFileDialog :: Window -> IORef String -> IO ()
openFileDialog parentWindow fileOpenPathIO = do
    dialog <- fileChooserDialogNew 
                    (Just "Select a City")    -- title of the window
                    (Just parentWindow)       -- the parent window
                    FileChooserActionOpen     -- the kind of dialog we want
                    [("gtk-cancel"            -- the buttons to display
	                 , ResponseCancel)
	                 ,("gtk-open"                                  
	                 , ResponseAccept)]
    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept      -> do filePath <- fileChooserGetFilename dialog
                                  let path = case filePath of
                                        (Just s   ) -> s
                                        Nothing     -> error $"Error on "
                                                       ++"ResponseAccept in "
                                                       ++"openFileDialog"
                                  modifyIORef fileOpenPathIO (\_ -> path)
        ResponseCancel      -> return ()
        ResponseDeleteEvent -> return ()
        _                   -> return ()
    widgetHide dialog
    

-------------------------------- < saveFileDialog > ---------------------------
saveFileDialog :: Window -> IORef String -> IO ()
saveFileDialog parentWindow fileSavePathIO = do
    dialog <- fileChooserDialogNew 
                    (Just "City will be saved as:")-- title of the window
                    (Just parentWindow)            -- parent window
                    FileChooserActionSave          -- kind of dialog we want
                    [("gtk-cancel"                 -- buttons to display
	                 , ResponseCancel)
	                 ,("gtk-save"                                  
	                 , ResponseAccept)]
    widgetShow dialog
    resp <- dialogRun dialog
    case resp of
        ResponseAccept      -> do filePath <- fileChooserGetFilename dialog
                                  let path = case filePath of
                                        (Just s   ) -> s
                                        Nothing     -> error $"Error on "
                                                       ++"ResponseAccept in "
                                                       ++"openFileDialog"
                                  modifyIORef fileSavePathIO (\_ -> path)
        ResponseCancel      -> return ()
        ResponseDeleteEvent -> return ()
        _                   -> return ()
    widgetHide dialog
    

-------------------------------------------------------------------------------
-- Returns the lowest size of the drawingarea

dynSize :: IORef City -> IO (Int, Int)
dynSize cityIO = do 
    liftIO $ do
    c <- readIORef cityIO
    let fieldWidth = getCityWidth c
        fieldHeight = getCityHeight c
        winW = if minWidth< spaceCell * fieldWidth
                  then spaceCell * fieldWidth
                  else minWidth
        winH = if minHeight< spaceCell * fieldHeight
                  then spaceCell * fieldHeight
                  else minHeight
    return (winW, winH)


-------------------- < showRandDialog > -----------------------------

showRandDialog :: GladeXML -> IORef [Int] -> IO ()
showRandDialog xml randValIO = do
    randDialog <- xmlGetWidget xml castToDialog "randomDialog"
    [widthSpin,heightSpin,horStreets,vertStreets,
     signals,buildings,cars,minLength,maxLength] <- mapM 
                (xmlGetWidget xml castToSpinButton) 
                ["spinWidth","spinHeight","horStreets","vertStreets",
                 "signals","buildings","cars","minLengthSpin",
                 "maxLengthSpin"]

-- setts the value and the sensitivity
    mapM_ (flip widgetSetSensitivity True) 
        [widthSpin,heightSpin,signals]
{-    mapM_ (flip widgetSetSensitivity False) 
        [horStreets,vertStreets,signals,buildings,cars]
    mapM_ (flip spinButtonSetValue 0) [signals,buildings,cars]
    mapM_ (flip spinButtonSetValue 1) 
        [horStreets,vertStreets]
    mapM_ (flip spinButtonSetValue 4) [widthSpin,heightSpin]-}

    widgetShow randDialog

-- the events  
-- width spinning box  
-- It reads the value of the spinbutton, activates the road spin 
-- buttons, if width and height was changed
    _ <- afterValueSpinned widthSpin $do 
        valWi <- spinButtonGetValueAsInt widthSpin
        valHe <- spinButtonGetValueAsInt heightSpin
        valHo <- spinButtonGetValueAsInt horStreets
        valVe <- spinButtonGetValueAsInt vertStreets
        spinButtonSetRange 
            vertStreets 1 
            $realToFrac (div valWi 4)
        spinButtonSetRange 
            buildings 0 
            $ realToFrac $2*valWi*valHo + 2*valHe*valVe - 16*valVe*valHo
        range <- spinButtonGetRange buildings
        if snd range > 0
            then mapM_ (flip widgetSetSensitivity True)
                [buildings]
            else mapM_ (flip widgetSetSensitivity False)
                [buildings,cars]
        if valWi >= 8
            then mapM_ (flip widgetSetSensitivity True)
                [vertStreets]
            else mapM_ (flip widgetSetSensitivity False)
                [vertStreets]


-- height spinning box  
    _ <- afterValueSpinned heightSpin $do 
        valWi <- spinButtonGetValueAsInt widthSpin
        valHe <- spinButtonGetValueAsInt heightSpin
        valHo <- spinButtonGetValueAsInt horStreets
        valVe <- spinButtonGetValueAsInt vertStreets
        spinButtonSetRange
            horStreets 1   
            $realToFrac (div valHe 4)
        spinButtonSetRange 
            buildings 0 
            $ realToFrac $2*valWi*valHo + 2*valHe*valVe - 16*valVe*valHo
        range <- spinButtonGetRange buildings
        if snd range > 0
            then mapM_ (flip widgetSetSensitivity True)
                [buildings]
            else mapM_ (flip widgetSetSensitivity False)
                [buildings,cars]
        if valHe >= 8
            then mapM_ (flip widgetSetSensitivity True)
                [horStreets]
            else mapM_ (flip widgetSetSensitivity False)
                [horStreets]


-- a number of the horizontal streets in the city
    _ <- afterValueSpinned horStreets $do
        valWi <- spinButtonGetValueAsInt widthSpin
        valHe <- spinButtonGetValueAsInt heightSpin
        valHo <- spinButtonGetValueAsInt horStreets
        valVe <- spinButtonGetValueAsInt vertStreets
        spinButtonSetRange
            signals 0
            $ realToFrac $valVe*valHo
        spinButtonSetRange 
            buildings 0 
            $ realToFrac $2*valWi*valHo + 2*valHe*valVe - 16*valVe*valHo
        range <- spinButtonGetRange buildings
        if snd range > 0
            then mapM_ (flip widgetSetSensitivity True)
                [buildings]
            else mapM_ (flip widgetSetSensitivity False)
                [buildings,cars]
            
            
-- a number of the vertical streets in the city
    _ <- afterValueSpinned vertStreets $do
        valWi <- spinButtonGetValueAsInt widthSpin
        valHe <- spinButtonGetValueAsInt heightSpin
        valHo <- spinButtonGetValueAsInt horStreets
        valVe <- spinButtonGetValueAsInt vertStreets
        spinButtonSetRange
            signals 0
            $ realToFrac $valVe*valHo
        spinButtonSetRange 
            buildings 0 
            $ realToFrac $2*valWi*valHo + 2*valHe*valVe - 16*valVe*valHo
        range <- spinButtonGetRange buildings
        if snd range > 0
            then mapM_ (flip widgetSetSensitivity True)
                [buildings]
            else mapM_ (flip widgetSetSensitivity False)
                [buildings,cars]
            


-- a number of the buildings in the city
    _ <- afterValueSpinned buildings $do
        valBuild <- spinButtonGetValueAsInt buildings
        spinButtonSetRange
            cars 0
            $realToFrac $valBuild
        range <- spinButtonGetRange cars
        if snd range > 0
            then mapM_ (flip widgetSetSensitivity True)
                [cars]
            else mapM_ (flip widgetSetSensitivity False)
                [cars]


-- the signal spinnbutton was pressed
    _ <- afterValueSpinned signals $do
        valSign <- spinButtonGetValueAsInt signals
        if valSign > 0
            then mapM_ (flip widgetSetSensitivity True)
                [minLength,maxLength]
            else mapM_ (flip widgetSetSensitivity False)
                [minLength,maxLength]

-- the minLength spinnbutton was pressed
    _ <- afterValueSpinned minLength $do
        valMin <- spinButtonGetValue minLength
        valMax <- spinButtonGetValue maxLength
        if valMin > valMax
            then spinButtonSetValue maxLength valMin
            else return ()

-- the maxLength spinnbutton was pressed
    _ <- afterValueSpinned maxLength $do
        valMin <- spinButtonGetValue minLength
        valMax <- spinButtonGetValue maxLength
        if valMax < valMin
            then spinButtonSetValue minLength valMax
            else return ()



-- the response action
    resp <- dialogRun randDialog
    case resp of 
        ResponseAccept -> do
            list <- mapM
                spinButtonGetValueAsInt
                [widthSpin,heightSpin,horStreets,vertStreets,
                signals,buildings,cars,minLength,maxLength]
            modifyIORef randValIO $return list
        ResponseReject  -> return ()
        _               -> return ()

    widgetHide randDialog


