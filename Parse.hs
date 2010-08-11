module Parse where
import AStar
import Data.List (isInfixOf, nub, (\\), sort, intersect)
import Data.IORef
import Char (isSpace)
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomR, StdGen)

-------------------------- < doInputString > ----------------------------------
-- doInputString get a String from the function readFile, makes one String for
-- each row and will also remove the space between the words/chars. After that  
-- it removes the commentlines and the empty lines of the String.

-- rmSpace remove the spaces and rmUnnecessaryLine remove the commandlines and
-- the empty lines. stringListBreakAt builds a String list for each element,
-- what is delimited by the given char.

--creates a String without the comments and the clear lines
doInputString :: String -> [String]
doInputString file = do let string = map rmSpace $lines file
                        rmUnnecessaryLines string

-- remove the clear lines and the comment lines
rmUnnecessaryLines :: [String] -> [String]
rmUnnecessaryLines input = 
    filter (\x -> (length x > 0) && ((head x) /='-')) input

-- remove the space in the strings
rmSpace :: String -> String
rmSpace s = filter (\x -> not (isSpace x)) s



-------------------------- < parse > ------------------------------------------
-- Parser gets the string from doInputString and builds the city data. First it
-- will extract the city dimensions, after this it creates four lists with
-- the roads, the buildings, the signals and the cars. In the data City the
-- dimensions and the lists will be stored. With the dimensions and the lists, 
-- the function buildCity build the data City.
parse :: IORef StdGen -> [String] -> City
parse genIO s = do 
    buildCity cityW cityH (roadCells++buildingCells)
                          signalCells
                          carCells
    where cityW = getDim s "width="
          cityH = getDim s "height="
          roadCells = concatMap generateRoad (searchRows s ":Roads")
          buildingCells = map generateBuilding
                              (searchRows s ":Buildings")
          signalCells = generateSignals (searchRows s ":Signals")
          carCells = map (generateCarList
                          buildingCells
                          roadCells genIO)
                         (searchRows s ":Cars")

-- returns the width or the height of the city
-- e.g. from this line: "width=10" as a String to "10" as an Int
-- where string must be "width="
getDim :: [String] -> String -> Int
getDim stringList string = 
    read (drop (length string) $head dimElement)
    where dimElement = filter (\x -> isInfixOf string x) stringList


-- returns only the rows between the selected string s and ":end"
-- s must be one element from {":Roads", ":Buildings", ":Signals", 
--  ":Cars"}
searchRows :: [String] -> String -> [String]
searchRows list s =
    --returns the string until ":end" is reached
    fst $ break (==":end") begin               
    where 
          --search for the string s, splits the [a] list in to two lists as a
          --([a],[a]) tuple, where s is in the second list. After that it tooks
          --the second list without the s element on the first position.
          begin = tail $snd $ break (==s) list 

                                  

-------------------------- < generateRoad > -----------------------------------
-- generateRoad gets the road strings and build the single roadcells between 
-- two points for both directions. The Cell contians the street id, the name of
-- the street and each cell knows the next cell in its direction. But the last
-- cell in the direction does not know the next cell for the backward direction.
-- This will be generated later.
generateRoad :: String -> [(Pos, Cell)]
generateRoad s = fillRoadCell (read idString) roadName (buildRoadList pos)
    where list = stringListBreakAt s ';'
          idString = head list
          roadName = list!!1
          pos = createPosTuple $list!!2
          

--build one list for the way forth and one list for the way back
buildRoadList :: [Pos] -> [[Pos]]
buildRoadList pos  = if null pos
                        then []
                        else buildRoadPointToPoint (pos!!0) (pos!!1) :
                                 buildRoadList (drop 2 pos) 


--build a list of roadpoints for one direction of one road.
buildRoadPointToPoint :: Pos -> Pos -> [Pos]
buildRoadPointToPoint (x1,y1) (x2,y2) =
    if (x1 <= x2)&&(y1<=y2)
       then [(x,y) | x <- [x1..x2], y <- [y1..y2]]
       else reverse [(x,y) | x <- [x2..x1], y <- [y2..y1]]


-- Fills the road cell with informations. i will be the identifier, s will be 
-- the name of the street and the next roadpiece will be at the next position 
-- of the road in one direction.
fillRoadCell :: Int -> String -> [[Pos]] -> [(Pos, Cell)]
fillRoadCell i s pos = concatMap (findNextRoad i s) pos


findNextRoad :: Int -> String -> [Pos] -> [(Pos, Cell)]
findNextRoad _ _ [] = []
findNextRoad i s (pos:xs) = (pos, Road i s nextR) : findNextRoad i s xs
                             where nextR = if null xs
                                              then []
                                              else [head xs]

-------------------------- < generateBuilding > -------------------------------

-- divide the buildingstring into three pieces. First piece is the id of the 
-- building, second piece is the name of the houseowners and the last piece 
-- will be the location of the building.
generateBuilding :: String -> (Pos, Cell)
generateBuilding x = generateBuildingList pos (read first) second
                        where list = stringListBreakAt x ';'
                              first = head list
                              second = list!!1
                              pos = createPosTuple $list!!2



-- builds the List for the buildings and fill the cell with informations
generateBuildingList :: [Pos] -> Int -> String -> (Pos,Cell)
generateBuildingList x i s = (head x, Building i s) 



-------------------------- < generateSignals > --------------------------------
-- Generates the signallist. The relationships between the signals works with
-- the positions of the signals. Each signal knows other signals, who work as
-- itself (list workWith) and who work different to itself (list workAgainst).
-- Example: If the signal is green, then the signals in workWith are also
-- green. But the singals in workAgainst are red!
generateSignals :: [String] ->[(Pos, Cell)]
generateSignals s = map (flip buildRelationships listCell) listCell
                        where listCell = map genListCell s


-- build a list of cells without relationships. stringListBeforeChar returns
-- a list which divided a string into substrings at the point of the ';' char.
-- Each new string will be reworked to an integer or to a position. Then a
-- signal cell will be createt with only its id. The result of the function
-- will be a position, a cell and two lists with id's. First list (wW) 
-- comprised the workWith signalid's and the second list (wA) comprised the
-- workAgainst signalid's.
genListCell :: String -> (Pos, Cell, [Int], [Int])
genListCell x = (pos, (Signal idSig statusSig step step [] []), wW, wA) 
                            where a = stringListBreakAt x ';'
                                  idSig = read (head a) :: Int
                                  pos = head $createPosTuple (a!!1)
                                  statusSig = "green" == (a!!2)
                                  step = read (a!!3) :: Int
                                  wW = getIntRel (a!!4)
                                  wA = getIntRel (a!!5)


-- makes the relationship id's from a string to an integer list
-- It gets a string like "[3,1]" and builds an integer list [3,1]
getIntRel :: String -> [Int]
getIntRel s = map read (stringListBreakAt withoutBrakets ',') 
              where withoutBrakets = filter (\x -> x /= '[' && x /= ']') s



-- builds the realationships between the signals. It will get two lists,
-- because the returned cell needs the coordinates of the other cells.
-- First list is the source for the returning cell and the second list is to
-- find the coordinates of the cells with the right id. The id's are stored
-- in the two integerlists. First integer-list is the workWith-list and
-- the second integer-list is the workAgainst-list
buildRelationships :: (Pos, Cell , [Int], [Int])   -> 
                      [(Pos, Cell , [Int], [Int])] -> 
                      (Pos,Cell)
buildRelationships ((xy,Signal idS st sA sR _ _, wWId, wAId)) l =
    (xy, Signal {ident=idS, 
                 status = st, 
                 stepToWait=sA,
                 remainingSteps= sR,
                 workWith = with,
                 against = workAgainst})
    where with = map (findPartner l) wWId
          workAgainst = map (findPartner l) wAId
buildRelationships _ _ = error $"Not a Signal in"
                                 ++"buildRelationships!"


--find the partner cell with the id in the second list named ids and 
--returns the position coordinates
findPartner :: [(Pos, Cell, [Int], [Int])] -> Int -> Pos
findPartner cell identSig =
    pos
    where (pos, _, _, _) = head $filter (\(_, Signal idd _ _ _ _ _, _, _) -> 
                                                                idd==identSig)
                                        cell





-------------------------- < generateCarList > --------------------------------
-- It extracts the id, the startposition and the endposition from the
-- stringlist. If there was only an id of a house, then it will be searching
-- the nearest road of the house. Then this roadpiece will be the start
-- position of the car. Then the colour of the car will be calculated randomly
-- with the function genRandomoColour. After all the cell will be created
generateCarList :: [(Pos, Cell)] ->
                   [(Pos, Cell)] -> 
                   IORef StdGen  ->
                   String        ->
                   (Pos, Cell)
generateCarList building roads genIO x = do
    (pos, (Car idR end [] [] col))
    where a = stringListBreakAt x ';'
          idR = read (head a) ::Int
          pos = findPos building roads (a!!1)
          end = findPos building roads (a!!2)
          col = darkerColours $genRandomColour genIO



-- it generates a random tuple with rgb values. Therefore it takes a standart
-- generator and generates new double values and a new generator
genRandomColour :: IORef StdGen -> (Double, Double, Double)
genRandomColour genIO = 
    unsafePerformIO $liftIO $ do
    gen <- readIORef genIO
    let (r, newGen1) = randomR (0,1::Double) gen
        (g, newGen2) = randomR (0,1::Double) newGen1
        (b, newGen3) = randomR (0,1::Double) newGen2
    modifyIORef genIO (\_ -> newGen3)
    return (r,g,b)



-- rebuild the colour values, if they are to brightly
darkerColours ::(Double, Double, Double) -> (Double, Double, Double)
darkerColours (r, g, b) = 
    if((r+b+g) *0.33)> barrier
        then (r-comp, g-comp, b-comp)
        else (r, g, b)
    where comp = 1-barrier
          barrier = 0.725



-- Extract the id of the house or the starting positon tuple
findPos ::  [(Pos, Cell)] -> [(Pos, Cell)] -> String -> Pos
findPos build street pos = if head pos=='H'
                            then findStreetForBuilding build
                                                       street 
                                                       (read $tail pos)
                            else head $createPosTuple pos



-- After the id of the house was found, it will search above, below, left and 
-- right of the house for a road. First roadpiece will be used as startingpoint
-- of the car.
findStreetForBuilding :: [(Pos, Cell)] -> [(Pos, Cell)] -> Int -> Pos
findStreetForBuilding build street idB = 
    head positions
    where buildingPos = [posB | (posB, _)<-
                            (filter (\(_,Building identB _) -> identB ==idB)
                            build)]
          positions = [posS | (posS, _) <- street, (x,y) <- buildingPos,
                       posS==(x,y-1)|| posS==(x-1,y) || posS==(x+1,y) ||
                       posS==(x,y+1)]





-------------------------- < buildCity > --------------------------------------
-- now the city will be created. The first list is like a two dimension array
-- each row is a seperate list in the list. That guaranteed the x and y
-- positions for the static list. And the second list is only a normal list,
-- which comprised the cars and the signals with the current positions.
buildCity :: Int -> 
             Int -> 
             [(Pos,Cell)] ->
             [(Pos,Cell)] ->
             [(Pos,Cell)] ->
             City
buildCity w h streetBuild signals cars = 
    City { width            = w,
           height           = h,
           static           = sB,
           dynamic          = signals++cars}
    where sB = buildList w h streetBuild
          
--the rows jointed together
buildList :: Int -> Int -> [(Pos,Cell)] -> [[Cell]]
buildList w h cl = [buildListRow w i cl | i <- [1..h]]

--each row a seperate list
buildListRow :: Int -> Int -> [(Pos,Cell)] -> [Cell]
buildListRow w h cl = [returnCellXY x h cl | x <- [1..w]]



-- returns the cell at x,y. But when nothnig is there, an empty cell will
-- be returned. It extracts also the buildings and the roads.
returnCellXY :: Int -> Int -> [(Pos,Cell)] -> Cell
returnCellXY x y list =
    if length cellList ==0
       then Empty {} 
       else case (head cellList) of 
                 { (_, Building identB nameB)-> Building identB nameB;
                   (_, Road _ _ _ )          -> buildDeadEndRoadJunction 
                                                    cellList
                                                    list;
                   (_, _) -> error "No dynamic Cells in returnCellXY!"
                 }
    where cellList = filter (\((x1,y1), _) -> x1==x && y1 == y) list


-- this one generates all deadends and roadjunctions. If only one cell is in
-- the list, then this one will be returned after checking for the next
-- road piece.
buildDeadEndRoadJunction :: [(Pos,Cell)] -> [(Pos,Cell)] -> Cell
buildDeadEndRoadJunction cellList list = 
    if length cellList == 1 
       then buildDeadEnd list (head cellList)
       else roadJunction (map (buildDeadEnd list) cellList)


-- the function search for the next road piece. If nothing was found, then
-- it searches for the nearest pice of the same road!
buildDeadEnd :: [(Pos,Cell)] -> (Pos,Cell) -> Cell
buildDeadEnd list ((x,y), Road idR nameR roadPath) = 
    if null roadPath
       then Road idR nameR [nextR]
       else Road idR nameR roadPath
    where 
          -- returns the four cells in the neighborhood.
          nearestStreetCells = 
            filter (\((x1,y1),_) -> (x1==x-1 || x1==x+1) && y1==y ||
                                     x1==x && (y1==y-1 || y1==y+1)
                   ) list 

          --returns a list with neighbors with the same streetids
          onlyThisStreetCells = 
            filter (\(_, Road identR _ _) -> identR == idR) 
                   nearestStreetCells 

          -- filters out the cell, which points on the current cell
          notBeforeThisCell = 
            filter (\(_, Road _ _ pathNext) -> (head pathNext) /= (x,y))
                   onlyThisStreetCells 

          -- next must be only one cell
          nextR = (\(pos,_) -> pos) (head notBeforeThisCell)  

buildDeadEnd _ _ = error "Cell must be a road cell in buildDeadEnd!"




-- build the roadjunctions. One junction will be four cells, and each cell
-- knows 2 directions. The next cell in direction and the left or right cell,
-- addicted by the direction of the cell on the right/left.
roadJunction :: [Cell] -> Cell
roadJunction roads = 
    Road idR nameRoad roadPath
    where idR = (\(Road identR _ _) -> identR) (head roads)
          nameRoad = (\(Road _ nameR _) -> nameR) (head roads)
          roadPath = nub (map (\(Road _ _ nextR) -> head nextR) roads)




-------------------------- < cityToString > -----------------------------------
-- This takes a City and builds a string to save this to a file
-- First it gets the width and the height of the city. After that
-- it joins the roads, signals, buildings and cars.
-- For more practicable, the static list will be ziped with 
-- coordinates.
cityToString :: City -> String
cityToString city =
    widthStr ++ heightStr ++ roads ++ signals ++ buildings ++ cars
    where 
          widthStr = "width = " ++ show widthC ++ "\n"
          heightStr = "height = " ++ show heightC ++ "\n"
          roads = ":Roads\n" ++ roadString posStat ++ end
          signals = ":Signals\n" ++ signalString dynCity ++ end
          buildings = ":Buildings\n" ++ buildingString posStat ++ end
          cars = ":Cars\n" ++ carString dynCity ++ end
          end = ":end\n"
          widthC = getCityWidth city
          heightC = getCityHeight city
          dynCity = getCityDynamic city
          statCity = getCityStatic city
          posStat = zip array $concat statCity
          array = [(y,x) | x <- [1..heightC], y <- [1..widthC]]
          



-------------------------- < roadString > -------------------------------------
-- This one gets the static cells with the coordinates, filters the 
-- buildings and the empty cells out, so that there are only road
-- cells available. Then the first road will be filtered out by the 
-- identifier. After creating the string, the list will be cleaned 
-- from the worked road cells and do it from the beginning.
roadString :: [(Pos,Cell)] -> String
roadString [] = []
roadString list = 
    show idR ++ ";" ++ nameR ++ ";" ++ getRoadPos filteredRoads ++ "\n"
      ++ (roadString $roads\\filteredRoads)
    where 
          roads = filter (\(_,cell) -> case cell of 
                                    {(Road _ _ _) -> True;
                                     _            -> False}) list
          idR = (\(_,Road id1 _ _) -> id1) $ head roads
          nameR = name $ snd $ head roads
          filteredRoads = roadWithSameId idR roads




-- This is a little bit tricky:
-- All vertikal streets were rebuild right, because the vertical
-- streets will be preferred by the parser, if two roads are on 
-- the same position.

-- The first road piece will always be the upper left piece.
-- For the vertical roads the first coordinate will be the position
-- from this first piece. The second position will be the last piece 
-- of all pieces in the same vertical line. The other two pieces
-- are only the first two plus 1 number in x-value

-- The same thing for the horizontal roads. But with a little
-- difference. All points will be checked, if the nextRoad
-- piece in nextRoad is the same, as the piece one y step downward
-- or one x step to the right. This is the left and right
-- correction of the end of the road.
getRoadPos :: [(Pos,Cell)] -> String
getRoadPos list = 
    -- checks, if it's a horizontal or vertical street
    -- if otherCellX > 2 then it's a vertical street
    if length otherCellX >2
       then show pos1V ++ show pos2V ++ show pos3V ++ show pos4V
       else show pos1H ++ show pos2H ++ show pos3H ++ show pos4H
    where (x1,y1) = fst $ head list
          (x2,y2) = fst $ head $ reverse otherCellY
          firstCell = snd $ head list
          lastCellYPlus1 = snd $ head $ reverse otherCellYPlus1
          -- filters the vertical street pieces with position (x,y0) to (x,yn)
          otherCellX = filter (\((xX,_),_) -> xX == x1) list
          -- filters the horizontal street pieces with pos (x0,y) to (xn,y)
          otherCellY = filter (\((_,yX),_) -> yX == y1) list
          otherCellYPlus1 = filter (\((_,yX),_) -> yX == y1 + 1) list
          -- e.g. (x1,y1) to (x1,y5) and return (x2,y5) to (x2,y1)+
          -- looks like this street:
          --  pos1V  pos4V
          --    |       |
          --    |       |
          --    |       |
          --  pos2V  pos3V          
          pos1V = (x1,y1) 
          pos2V = fst $head $reverse otherCellX
          pos3V = ((fst pos2V)+1, snd pos2V)
          pos4V = (x1+1,y1)
          -- horizontal streets looks like:
          --   pos4H-----------pos3H
          --   pos1H-----------pos2H          
          pos1H = (fst pos4H,(snd pos4H) + 1)
          pos2H = (fst pos3H,1+snd pos3H)
          pos3H = if (nextRoad lastCellYPlus1) /= [(x2, y2)]
                     -- the road ends before a junction with a verticla
                     -- street, so it must go 2 fields to the right to
                     -- connect the other street. Remember that a junction
                     -- belongs to both streets!
                     then (2 + x2, y2)
                     else (x2,y2)
          pos4H = if (nextRoad firstCell) /= [(x1,y1+1)]
                     -- the same as pos3H, but now we took 2 fiels
                     -- from the left
                     then (x1-2,y1)
                     else (x1,y1)



-- Filtered all road cells out, which have not the given id.
roadWithSameId :: Int -> [(Pos,Cell)] -> [(Pos,Cell)]
roadWithSameId idR1 list =
    filter (\(_, Road idR2 _ _) -> idR1 == idR2) list



-------------------------- < signalString > -----------------------------------
-- Generates a string for the signals
signalString :: [(Pos,Cell)] -> String
signalString list = 
    concatMap (\(pos, Signal
                      idS
                      sta
                      steps
                      _
                      workWithS
                      workAgainst)
                        -> show idS ++ ";" ++
                           show pos ++ ";" ++
                           statusS sta ++ ";" ++
                           show steps ++ ";" ++
                           show (otherSigId workWithS list) ++ ";" ++
                           show (otherSigId workAgainst list) ++ "\n"
            ) signals
    where   
          signals = filter (\(_,cell) -> case cell of 
                                {(Signal _ _ _ _ _ _) -> True;
                                 _                    -> False}) list
          statusS x = if x then "green" else "red"

-- Returns the id for the signals addicted to the given signal at the position
-- comprise in poslist
otherSigId :: [Pos] -> [(Pos,Cell)] -> [Int]
otherSigId poslist list = 
    map (\(_,Signal idS _ _ _ _ _) -> idS) $ concat otherSignals
    where
          otherSignals = map (\x ->  filter (\(pos,_) -> x == pos) list)
                             poslist
         
-------------------------- < buildingString > ---------------------------------
-- generates a string for the buildings
buildingString :: [(Pos,Cell)] -> String
buildingString list =
    concatMap (\(pos, Building idB nameB) -> show idB ++ ";" ++
                                             nameB ++ ";" ++
                                             show pos ++ "\n") buildings
    where 
           buildings = filter (\(_,cell) -> case cell of 
                                    {(Building _ _) -> True;
                                     _              -> False}) list

-------------------------- < carString > --------------------------------------
-- generates a string for the cars
carString :: [(Pos,Cell)] -> String
carString list = 
    concatMap (\(pos,Car idC destC _ _ _ ) -> show idC ++ ";" ++
                                              show pos ++ ";" ++ 
                                              show destC ++ "\n") cars
    where 
          cars = filter (\(_,cell) -> case cell of
                             {(Car _ _ _ _ _) -> True;
                               _              -> False}) list



-------------------------- < randomCity > -------------------------------------
-- It generates a random city from the randomCityDialog
-- Therefor it takes the integer values and builds a string like a readFile 
-- string. 
randomCity :: [Int] -> IORef StdGen -> String
randomCity (cityWidth:cityHeight:hor:vert:
 sign:build:carsNumb:minL:maxL:[])
 genIO = 
    "width = " ++ show cityWidth ++ "\nheight = " ++
    show cityHeight ++ "\n:Roads\n" ++ unlines roadStr ++ 
    ":end\n:Signals\n" ++ signals ++
    ":end\n:Buildings\n" ++ buildings ++ ":end\n" ++ ":Cars\n" ++
    cars ++ ":end"
    where -- the roads:
          horizontalRoads = map (getRoads cityWidth 
                             cityHeight hor True genIO) 
                             [1..hor]
          verticalRoads = map (getRoads cityHeight 
                             cityWidth vert False genIO)
                             [1..vert]
          roadStr = map (\(i ,(w,x,y,z)) ->
            show i ++ ";" ++ show i ++ ";" ++ show w ++ show x ++
            show y ++ show z) $ verticalRoads ++ horizontalRoads


          -- the signals:
          -- first it generates the horizontal and the vertical streetpositions
          horRoadList = concatMap buildListFromTuple horizontalRoads
          vertRoadList = concatMap buildListFromTuple verticalRoads
          -- after that it filters the roadjunctionpositions and 
          -- builds the signals
          junctions = sort $intersect vertRoadList horRoadList
          signals = getSignals junctions sign minL maxL genIO


          -- the buildings:
          -- all possible building positions:
          rawBuildingList = 
            nub$
            concat $(map maybeBuildingPositions horRoadList)++
                    (map maybeBuildingPositions vertRoadList)
          -- all possible building positions without the roads
          buildListWithSignPos = rawBuildingList \\
                         (horRoadList++vertRoadList)
          -- all possible building positions without the maybe signal positions
          buildListWithErrorPos = buildListWithSignPos \\
            (concatMap maybeBuildingPositions junctions)
          -- all possible building positions without the positions out  
          -- of the border
          buildingList = filter (\(x,y) -> x >= 1 && x <= cityWidth &&
                                           y >= 1 && y <= cityHeight)
                                buildListWithErrorPos
          buildingTuple = getBuildings (sort buildingList) build genIO
          buildings = unlines $
                map (\(i,pos) -> show i ++ ";" ++ show i ++ ";" ++
                                 show pos) buildingTuple


          -- the cars:
          cars = unlines $ getCars buildingTuple buildingTuple carsNumb genIO
randomCity _ _ = error "The random city gets an unknwon list!"



-- returns random positions of the streets
getRoads :: Int -> Int -> Int -> Bool ->
            IORef StdGen -> Int -> (Int,(Pos,Pos,Pos,Pos))
getRoads x y z horizontal genIO streets =
  unsafePerformIO $ liftIO $do
    gen <- readIORef genIO
    let 
        -- if it's a horizontal street, then the id is the number of the street
        -- if not, then the vertical street id is added by the number of the 
        -- horizontal streets. That prevents double ids
        streetId = if horizontal
                  then streets
                  else streets+x
        -- The first position for the horizontal streets is, shown from 
        -- the upper left corner, one under the left upper corner of the street
        -- like :
        -- pos4-----------------------------------pos3
        -- |                                         |
        -- pos1-----------------------------------pos2
        pos1 = if horizontal 
                  then 1 + streets * distance - randDist
                  else streets * distance - randDist
        -- The distance is the maximum distance between two streets.
        -- If the field is 20 long and comprised 4 streets, the maximum
        -- distance between the roads are 5.
        -- Every 5 pieces containing one road.
        distance = div y z
        -- distance -2, because on the left and right side of the street 
        -- should be a house.
        (randDist,genNew) = randomR (2,distance-2) gen
        position = if horizontal 
            then ((1,pos1),(x,pos1),(x,(pos1 - 1)),(1,(pos1-1)))
            else ((pos1,1),(pos1, x),((pos1 + 1),x),((pos1+1),1))
    modifyIORef genIO (\_ -> genNew)
    return (streetId ,position)

-- generates the whole street
buildListFromTuple :: (Int,(Pos,Pos,Pos,Pos)) -> [Pos]
buildListFromTuple (_,(p1,p2,p3,p4)) =
    buildRoadPointToPoint p1 p2 ++
    buildRoadPointToPoint p3 p4



-- from the list of the whole junctions it selects a random roadpiece
-- After that it rebuilds the whole junction, generates a random
-- signallength, an id and build the string for the parser.
-- Then it will extract the used junction from the list and reduces
-- the counter that is shown, how much signal must be generated.
getSignals :: [Pos] -> Int -> Int -> Int -> IORef StdGen -> String
getSignals _ 0 _ _ _= []
getSignals [] _ _ _ _= []
getSignals list i minL maxL genIO = 
  unsafePerformIO $ liftIO $do
    gen <- readIORef genIO
    let (pos,genNew) = randomR (0, (length list) - 1) gen
        -- (x,y) at a random position in list
        (x,y) = list!!pos
        -- get all 4 junction filds for this junction
        maybeJunctionFields = [(x-1,y-1),(x,y-1),(x+1,y-1),
                               (x-1,y)  ,(x,y  ),(x+1,y  ),
                               (x-1,y+1),(x,y+1),(x+1,y+1)]
        junction = sort $ intersect list maybeJunctionFields
        -- the signal length
        (signalLength,genNew2) = randomR (minL,maxL) genNew
        -- the signal positions
        sig1Pos = show ((fst $junction!!0)-1,(snd $junction!!0)-1)
        sig2Pos = show ((fst $junction!!1)-1,(snd $junction!!1)+1)
        sig3Pos = show ((fst $junction!!2)+1,(snd $junction!!2)-1)
        sig4Pos = show ((fst $junction!!3)+1,(snd $junction!!3)+1)
        -- the signal ids
        sig1Id = show (i*4)
        sig2Id = show (i*4+1)
        sig3Id = show (i*4+2)
        sig4Id = show (i*4+3)
        sigLength = show signalLength
        -- the string
        sig1 = sig1Id++";"++sig1Pos++";green;"++sigLength++";["++
               sig4Id++"];["++sig2Id++","++sig3Id++"]\n"
        sig2 = sig2Id++";"++sig2Pos++";red;"++sigLength++";["++
               sig3Id++"];["++sig1Id++","++sig4Id++"]\n"
        sig3 = sig3Id++";"++sig3Pos++";red;"++sigLength++";["++
               sig2Id++"];["++sig1Id++","++sig4Id++"]\n"
        sig4 = sig4Id++";"++sig4Pos++";green;"++sigLength++";["++
               sig1Id++"];["++sig2Id++","++sig3Id++"]\n"
    modifyIORef genIO (\_ -> genNew2)
    return $sig1++sig2++sig3++sig4++
            getSignals (list\\junction) (i-1) minL maxL genIO


-- The positions around the road cell will be maybe used as positions
-- for a house
maybeBuildingPositions :: Pos -> [Pos]
maybeBuildingPositions (x,y) = 
    (x-1,y-1):(x,y-1):(x+1,y-1):
    (x-1,y  ):(x,y  ):(x+1,y  ):
    (x-1,y+1):(x,y+1):(x+1,y+1):[]



-- generates the buildings from the possible positions
getBuildings :: [Pos] -> Int -> IORef StdGen -> [(Int,Pos)]
getBuildings _ 0 _ = []
getBuildings list i genIO = unsafePerformIO $ liftIO $ do
    gen <- readIORef genIO
    let (randPos,genNew) = randomR (0, (length list)-1) gen
        pos = list!!randPos
    modifyIORef genIO (\_ -> genNew)
    return $(i,pos):getBuildings (list\\[pos]) (i-1) genIO




-- generates the cars
getCars :: [(Int,Pos)] -> [(Int,Pos)] -> Int -> IORef StdGen -> [String]
getCars _ _ 0 _ = []
getCars sourceList destList i genIO = unsafePerformIO $ liftIO$ do
    gen <- readIORef genIO
    let 
        -- it takes a random house for the start position
        (randPos, genNew) = randomR (0,(length sourceList)-1) gen
        posTup = sourceList!!randPos
        listNewSource = sourceList\\[posTup]
        -- builds the start position string
        pos = "H"++(show $fst $posTup)
        -- takes a destinatino position from the destination list
        (randEnd, genNew2) = randomR (0,(length destList)-1) genNew
        endTup = destList!!randEnd
        -- builds the destination string and the id string
        end = "H"++(show $fst endTup)
        identCar = show i
    modifyIORef genIO (\_ -> genNew2)
    return $ (identCar++";"++pos++";"++end):
            getCars listNewSource (destList\\[endTup]) (i-1) genIO
