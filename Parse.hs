module Parse where
import AStar
import Data.List (isInfixOf, nub, (\\), sort, intersect)
import Data.IORef
import Char (isSpace)
import Control.Monad.Trans (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomR, StdGen)

-------------------------------------------------------------------------------
-- doInputString get a String from the function readFile, makes one String for
-- each row and it will also remove the space between the words/chars, it 
-- remove the commentlines and the empty lines of the String.

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
rmSpace s =filter (\x -> not (isSpace x)) s


-- builds a stringlist from a string. The new list element will beginn after
-- the given char.
stringListBreakAt :: String -> Char -> [String]
stringListBreakAt string char =
    lines $ map (\x -> if x == char then '\n' else x) string




-------------------------------------------------------------------------------
-- Parser gets the string from doInputString and builds the city data. First it 
-- will extract the city dimensions, after this it create the roads, the 
-- buildings, the signals and the cars. In the data City will be stored the 
-- dimensions and two lists. First list
-- contain the streets and the buildings. The second list contain the signals 
-- and the cars.
parse :: IORef StdGen -> [String] -> City
parse genIO s = do 
    buildCity cityW cityH (roadCells++buildingCells)
                          signalCells
                          carCells
    where cityW = getDim s "width="
          cityH = getDim s "height="
          roadCells = concat $map generateRoad (searchRows s ":Roads")
          buildingCells = map generateBuilding
                              (searchRows s ":Buildings")
          signalCells = generateSignals (searchRows s ":Signals")
          carCells = map (generateCarList
                          buildingCells
                          roadCells genIO)
                         (searchRows s ":Cars")

-- returns the width or the height of the city
getDim :: [String] -> String -> Int
getDim stringList string = 
    read (drop (length string) $head dimElement)
    where dimElement = filter (\x -> isInfixOf string x) stringList


--return only the rows between the selected string s and ":end"
searchRows :: [String] -> String -> [String]
searchRows list s =
    --returns the string until ":end" is reached
    fst $ break (==":end") begin               
    where 
          --read the strings after the string s
          begin = tail $snd $ break (==s) list 

                                  

-- turns a tuplestring to tuples. Get ["(1,1)(2,2)"] and return [(1,1),(2,2)] 
createPosTuple :: String -> [Pos]
createPosTuple s = 
    map (\str -> (read (fst $ break (==',') str) :: Int,
                  read (tail $ snd $ break (==',') str) :: Int))
        $ lines withoutBrakets
    where withoutBraketFst = filter (\x -> x /= '(') s
          withoutBrakets = map (\x -> if x ==')' then '\n' else x)
                               withoutBraketFst




-- generateRoad gets the road strings and build the single roadcells between 
-- two points. The Cell contian the street id and the name of the street
generateRoad :: String -> [(Pos, Cell)]
generateRoad x = fillRoadCell (read first) second (buildRoadList pos)
    where list = stringListBreakAt x ';'
          first = head list
          second = list!!1
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


-- fill the road cell with informations. i will be the identifier, s will be 
-- the name of the street and the next roadpiece will be at the next position 
-- of the road.
fillRoadCell :: Int -> String -> [[Pos]] -> [(Pos, Cell)]
fillRoadCell i s pos = concat $ map (findNextRoad i s) pos


findNextRoad :: Int -> String -> [Pos] -> [(Pos, Cell)]
findNextRoad _ _ [] = []
findNextRoad i s (pos:xs) = (pos, Road i s nextR) : findNextRoad i s xs
                             where nextR = if null(xs)
                                              then []
                                              else [head xs]

-------------------------------------------------------------------------------

-- divide the buildingstring into three pieces. First piece is the id of the 
-- building, second piece is the name of the houseowners and the last piece 
-- will be the location of the building.
generateBuilding :: String -> (Pos, Cell)
generateBuilding x = generateBuildingList pos (read first) second
                        where list = stringListBreakAt x ';'
                              first = head list
                              second = list!!1
                              pos = createPosTuple $list!!2



--builds the List for the buildings and fill the cell with informations
generateBuildingList :: [Pos] -> Int -> String -> (Pos,Cell)
generateBuildingList x i s = (head x, Building i s) 




-- generates the signallist. The relationships between the signals works with
-- the positions of the signals. Each signal knows other signals, who work as
-- itself (list workWith) and who work different to itself (list workAgainst).
-- Example: If the signal it green, then the signals in workWith are also
-- green. But the singals in workAgainst are red!
generateSignals :: [String] ->[(Pos, Cell)]
generateSignals s = buildRelationships listCell listCell
                        where listCell = map genListCell s


-- build a list of cells without relationships. stingListBeforeChar returns
-- a list which divided a string into substrings at the point of the ';' char.
-- Each new string will be reworked to an integer or to a position. Then a
-- signal cell will be createt with only its id. The result of the function
-- will be a position, a cell and two lists with id's. First list comprised
-- the workWith signalid's and the second list  comprised the workAgainst
-- signalid's.
genListCell :: String -> (Pos, Cell, [Int], [Int])
genListCell x = (head pos, (Signal idSig statusSig step step [] []), wW, wA) 
                            where a = stringListBreakAt x ';'
                                  idSig = read (head a) :: Int
                                  pos = createPosTuple (a!!1)
                                  statusSig = "green" == (a!!2)
                                  step = read (a!!3) :: Int
                                  wW = getIntRel (a!!4)
                                  wA = getIntRel (a!!5)


--makes the relationship id's from a string to an integer list
getIntRel :: String -> [Int]
getIntRel s = map readI (stringListBreakAt withoutBrakets ',') 
              where withoutBrakets = filter (\x -> x /= '[' && x /= ']') s


--returns an integer from a String
readI :: String -> Int
readI s = read s :: Int



-- builds the realationships between the signals. It will get two lists,
-- because the returned cell needs the coordinates of the other cells.
-- First list is the source for the returning cell and the second list is to
-- find the coordinates of the cells with the right id. The id's are stored
-- in the two integerlists. Firs integer-list is the workWith-list and
-- the second integer-list is the workAgainst-list
buildRelationships :: [(Pos, Cell , [Int], [Int])] -> 
                      [(Pos, Cell , [Int], [Int])] -> 
                      [(Pos,Cell)]
buildRelationships [] _ = []
buildRelationships ((xy,Signal idS st sA sR _ _, wId, aId):xs) l =
    (xy, Signal {ident=idS, 
                 status = st, 
                 stepToWait=sA,
                 remainingSteps= sR,
                 workWith = with,
                 against = workAgainst}): buildRelationships xs l
    where with = map (findPartner l) wId
          workAgainst = map (findPartner l) aId
buildRelationships ((_, _, _, _) : _) _ = error $"Not a Signal in"
                                                 ++"buildRelationships!"


--find the partner cell with the id in the second list named ids and 
--returns the position coordinates
findPartner :: [(Pos, Cell, [Int], [Int])] -> Int -> Pos
findPartner cell identSig =
    (\(pos, _, _, _) -> pos ) onePartner
    where onePartner = head $filter (\(_, Signal idd _ _ _ _ _, _, _) -> 
                                        idd==identSig)
                                        cell






-- build the car list and the path of the cars. It extracts the id,
-- the startposition and the endposition from the stringlist. If there was only
-- an id of a house, then it will be searching the nearest road of the house.
-- This roadpiece will be the start position of the car. Then the path of
-- the car will be calculated with the function makePath. After all the cell
-- will be created
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
    let r = randomR (0,1::Double) gen
        g = randomR (0,1::Double) $snd r
        b = randomR (0,1::Double) $snd g
    modifyIORef genIO (\_ -> snd b)
    return (fst r,fst g,fst b)



-- rebuild the colour values, if they are to brightly
darkerColours ::(Double, Double, Double) -> (Double, Double, Double)
darkerColours (r, g, b) = 
    if((r+b+g) *0.33)> barrier
        then (r-comp, g-comp, b-comp)
        else (r, g, b)
    where comp = 1-barrier
          barrier = 0.725



{- Extract the id of the house or the starting positon tuple-}
findPos ::  [(Pos, Cell)] -> [(Pos, Cell)] -> String -> Pos
findPos build street pos = if head pos=='H'
                            then findStreetForBuilding build
                                                       street 
                                                       (readI(tail pos))
                            else head(createPosTuple pos)



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



-------------------------------------------------------------------------------





-- now the city will be created. The first list is like a two dimension array
--  each row is a seperate list in the list. That guaranteed the x and y
-- positions. And the second list is only a normal list, which comprised
-- the cars and the signals with the current positions.
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
                 { (_, Building identB nameB)     -> Building identB nameB;
                   (_, Road _ _ _ )-> buildDeadEndRoadJunction cellList list;
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

          -- filters out the cell, which point on the current cell
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



---------------------------------- < cityToString >------------------
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

-- The first road piece will alwaly be the upper left piece.
-- For the vertical roads the first coordinate will be the position
-- from the firs piece. The second position will be the last piece 
-- of all pieces in the same vertical line. The other two pieces
-- are only the first two plus 1 number in x-value

-- The same thing for the horizontal roads. But with a little
-- difference. All points will be checked, if the nextRoad
-- piece in nextRoad is the same, as the piece one y step downward
-- or one x step to the right. This is the left and right
-- correction of the end of the road.
getRoadPos :: [(Pos,Cell)] -> String
getRoadPos list = 
    if length otherCellX >2
       then show pos1V ++ show pos2V ++ show pos3V ++ show pos4V
       else show pos1H ++ show pos2H ++ show pos3H ++ show pos4H
    where (x1,y1) = fst $ head list
          (x2, y2) = fst $ head $ reverse otherCellY
          firstCell = snd $ head list
          lastCellYPlus1 = snd $ head $ reverse otherCellYPlus1
          otherCellX = filter (\((xX,_),_) -> xX == x1) list
          otherCellY = filter (\((_,yX),_) -> yX == y1) list
          otherCellYPlus1 = filter (\((_,yX),_) -> yX == y1 + 1) list
          pos1V = (x1,y1) 
          pos2V = fst $head $reverse otherCellX
          pos3V = ((fst pos2V)+1, snd pos2V)
          pos4V = (x1+1,y1)
          pos1H = (fst pos4H,(snd pos4H) + 1)
          pos2H = (fst pos3H,1+snd pos3H)
          pos3H = if (nextRoad lastCellYPlus1) /= [(x2, y2)]
                     then (2 + x2, y2)
                     else fst $ head $ reverse otherCellY
          pos4H = if (nextRoad firstCell) /= [(x1,y1+1)]
                     then (x1-2,y1)
                     else (x1,y1)



-- Filtered all road cells out, which have not the given id.
roadWithSameId :: Int -> [(Pos,Cell)] -> [(Pos,Cell)]
roadWithSameId idR1 list =
    filter (\(_, Road idR2 _ _) -> idR1 == idR2) list



-- Generates a string for the signals
signalString :: [(Pos,Cell)] -> String
signalString list = 
    concat $ 
        map (\(pos, Signal
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

-- Returns the id for the signals addicted to the given signal
otherSigId :: [Pos] -> [(Pos,Cell)] -> [Int]
otherSigId poslist list = 
    map (\(_,Signal idS _ _ _ _ _) -> idS) $ concat otherSignals
    where
          otherSignals = map (\x ->  filter (\(pos,_) -> x == pos) list)
                             poslist
         

-- generates a string for the buildings
buildingString :: [(Pos,Cell)] -> String
buildingString list =
    concat $ map (\(pos, Building idB nameB) -> show idB ++ ";" ++
                                                nameB ++ ";" ++
                                                show pos ++ "\n") buildings
    where 
           buildings = filter (\(_,cell) -> case cell of 
                                    {(Building _ _) -> True;
                                     _              -> False}) list


-- generates a string for the cars
carString :: [(Pos,Cell)] -> String
carString list = 
    concat $ map (\(pos,Car idC destC _ _ _ ) -> show idC ++ ";" ++
                                                 show pos ++ ";" ++ 
                                                 show destC ++ "\n") cars
    where 
          cars = filter (\(_,cell) -> case cell of
                             {(Car _ _ _ _ _) -> True;
                               _              -> False}) list




---------------------------- < randomCity > -------------------------
randomCity :: [Int] -> IORef StdGen -> String
randomCity (cityWidth:cityHeight:hor:vert:
 sign:build:carsNumb:minL:maxL:[])
 genIO = 
    "width = " ++ show cityWidth ++ "\nheight = " ++
    show cityHeight ++ "\n:Roads\n" ++ unlines roadStr ++ 
    ":end\n:Signals\n" ++ signals ++
    ":end\n:Buildings\n" ++ buildings ++ ":end\n" ++ ":Cars\n" ++
    cars ++ ":end"
    where 
          horizontalRoads = map (getRoads cityWidth 
                             cityHeight hor True genIO) 
                             [1..hor]
          verticalRoads = map (getRoads cityHeight 
                             cityWidth vert False genIO)
                             [1..vert]
          roadStr = map (\(i ,(w,x,y,z)) ->
            show i ++ ";" ++ show i ++ ";" ++ show w ++ show x ++
            show y ++ show z) $horizontalRoads ++ verticalRoads
          horRoadList = concat $map buildListFromTuple horizontalRoads
          vertRoadList = concat $map buildListFromTuple verticalRoads
          junctions = sort $intersect vertRoadList horRoadList
          signals = getSignals junctions sign minL maxL genIO
          rawBuildingList = 
            nub$
            concat $(map maybeBuildingPositions horRoadList)++
                    (map maybeBuildingPositions vertRoadList)
          buildListWithSignPos = rawBuildingList \\
                         (horRoadList++vertRoadList)
          buildListWithErrorPos = buildListWithSignPos \\
            (concat $ map maybeBuildingPositions junctions)
          buildingList = filter (\(x,y) -> x >= 1 && x <= cityWidth &&
                                           y >= 1 && y <= cityHeight)
                                buildListWithErrorPos
          buildingTuple = getBuildings (sort buildingList) build genIO
          buildings = unlines $
                map (\(i,pos) -> show i ++ ";" ++ show i ++ ";" ++
                                 show pos) buildingTuple
          cars = unlines $ getCars buildingTuple buildingTuple carsNumb genIO
randomCity _ _ = error "The random city gets an unknwon list!"


maybeBuildingPositions :: Pos -> [Pos]
maybeBuildingPositions (x,y) = 
    (x-1,y-1):(x,y-1):(x+1,y-1):
    (x-1,y  ):(x,y  ):(x+1,y  ):
    (x-1,y+1):(x,y+1):(x+1,y+1):[]


buildListFromTuple :: (Int,(Pos,Pos,Pos,Pos)) -> [Pos]
buildListFromTuple (_,(p1,p2,p3,p4)) =
    buildRoadPointToPoint p1 p2 ++
    buildRoadPointToPoint p3 p4

-- returns random positions of the streets
getRoads :: Int -> Int -> Int -> Bool ->
            IORef StdGen -> Int -> (Int,(Pos,Pos,Pos,Pos))
getRoads x y z horizontal genIO streets =
  unsafePerformIO $ liftIO $do
    gen <- readIORef genIO
    let 
        streetId = if horizontal
                  then streets
                  else streets+x
        pos1 = if horizontal 
                  then 1 + streets * distance - randDist
                  else streets * distance - randDist
        distance = div y z
        (randDist,genNew) = randomR (2,distance-2) gen
        position = if horizontal 
            then ((1,pos1),(x,pos1),(x,(pos1 - 1)),(1,(pos1-1)))
            else ((pos1,1),(pos1, x),((pos1 + 1),x),((pos1+1),1))
    modifyIORef genIO (\_ -> genNew)
    return (streetId ,position)


-- from the list of the whole junctions it select a random roadpiece
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
        (x,y) = list!!pos
        maybeJunctionFields = [(x-1,y-1),(x,y-1),(x+1,y-1),
                               (x-1,y)  ,(x,y  ),(x+1,y  ),
                               (x-1,y+1),(x,y+1),(x+1,y+1)]
        junction = sort $ intersect list maybeJunctionFields
        (signalLength,genNew2) = randomR (minL,maxL) genNew
        sig1Pos = show ((fst $junction!!0)-1,(snd $junction!!0)-1)
        sig2Pos = show ((fst $junction!!1)-1,(snd $junction!!1)+1)
        sig3Pos = show ((fst $junction!!2)+1,(snd $junction!!2)-1)
        sig4Pos = show ((fst $junction!!3)+1,(snd $junction!!3)+1)
        sig1Id = show (i*4)
        sig2Id = show (i*4+1)
        sig3Id = show (i*4+2)
        sig4Id = show (i*4+3)
        sigLength = show signalLength
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
            

-- generates the buildings from the possible positions
getBuildings :: [Pos] -> Int -> IORef StdGen -> [(Int,Pos)]
getBuildings _ 0 _ = []
getBuildings list i genIO = unsafePerformIO $ liftIO $ do
    gen <- readIORef genIO
    let (randPos,genNew) = randomR (0, (length list)-1) gen
        pos = list!!randPos
    modifyIORef genIO (\_ -> genNew)
    return $(i,pos):getBuildings (list\\[pos]) (i-1) genIO


getCars :: [(Int,Pos)] -> [(Int,Pos)] -> Int -> IORef StdGen -> [String]
getCars _ _ 0 _ = []
getCars sourceList destList i genIO = unsafePerformIO $ liftIO$ do
    gen <- readIORef genIO
    let (randPos, genNew) = randomR (0,(length sourceList)-1) gen
        posTup = sourceList!!randPos
        listNewSource = sourceList\\[posTup]
        pos = "H"++(show $fst $posTup)
        (randEnd, genNew2) = randomR (0,(length destList)-1) genNew
        endTup = destList!!randEnd
        end = "H"++(show $fst endTup)
        identCar = show i
    modifyIORef genIO (\_ -> genNew2)
    return $ (identCar++";"++pos++";"++end):
            getCars listNewSource (destList\\[endTup]) (i-1) genIO
