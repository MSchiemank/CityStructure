module Parse where

import Data.List
import Char

--Datatype setting
data Cell = Road    { ident         :: Int,
                      name          :: String,
                      nextRoad      :: [Pos]}
          | Building{ ident         :: Int,
                      name          :: String}
          | Signal  { ident         :: Int,
                      status        :: Bool,
                      stepToWait    :: Int,
                      remainingSteps:: Int,
                      workWith      :: [Pos],
                      against       :: [Pos]}
          | Car     { ident         :: Int,
                      dest          :: Pos,
                      iWasThere     :: [Pos]}
          | Empty {}
     deriving (Eq, Show)

type Pos = (Int, Int)
data City = City { width              :: Int,
                   height             :: Int,
                   static             :: [[Cell]],
                   dynamic            :: [(Pos,Cell)]}
     deriving (Eq, Show)

--returns the contents of the structure
getCityStatic :: City -> [[Cell]]
getCityStatic (City width height static dynamic) = static

getCityDynamic :: City -> [(Pos,Cell)]
getCityDynamic (City width height static dynamic) = dynamic

getCityWidth :: City -> Int
getCityWidth (City width height static dynamic) = width

getCityHeight :: City -> Int
getCityHeight (City width height static dynamic) = height

{-------------------------------------------------------------
doInputString get a String from the function readFile and makes one String for each row.
It will also remove the space between the words/chars, it remove the commentlines and the empty lines of the String.

rmNullString remove the empty lines, rmSpace remove the spaces anf rmCommentLine remove the commandlines. stringListBeforeChar builds the String for each row and makeString will help to find the newline-char. -}

--creates a String without the comments and the clear lines
doInputString :: String -> [String]
doInputString file = do let string = lines file--stringListBeforeChar file '\n'
                        rmSpace (rmCommentLine (rmNullString string))


-- remove the clear lines
rmNullString :: [String] -> [String]
rmNullString lines = filter (\x -> length x > 0) lines

-- remove the space in the strings
rmSpace :: [String] -> [String]
rmSpace [] = []
rmSpace (x:xs) =[filter(\s -> not (isSpace s)) x] ++ rmSpace xs

-- remove the comment lines
rmCommentLine :: [String] -> [String]
rmCommentLine lines = filter (\x -> (head x) /='-') lines

-- builds a string with each listelement is one row of the file
stringListBeforeChar :: String -> Char -> [String]
stringListBeforeChar [] c = []
stringListBeforeChar file c = [row] ++ stringListBeforeChar (drop ((length row)+1)file) c
                 where row = makeString file c
              
-- selects the row of the file
makeString :: [Char] -> Char -> [Char]
makeString [] c = []
makeString (x:xs) c | x == c = []
                    | otherwise = [x] ++ makeString xs c




{------------------------------------------------------------}
{-Parser gets the string from doInputString and builds the city data. First it will
  extract the city dimensions, after this it create the roads, the buildings, the signals
  and the cars. In the data City will be stored the dimensions and two lists. First list
  contain the streets and the buildings. The second list contain the signals and the
  cars.-}
parse :: [String] -> City
parse s = do buildCity width height (roadCells++buildingCells) (signalCells++carCells)
             where width = getDim s "width="
                   height = getDim s "height="
                   roadCells = generateRoad (searchRows s ":Roads")
                   buildingCells = generateBuilding (searchRows s ":Buildings")
                   signalCells = generateSignals (searchRows s ":Signals")
                   carCells = generateCarList (searchRows s ":Cars") buildingCells roadCells

-- returns the width or the height of the city
getDim :: [String] -> String -> Int
getDim [] s = 1
getDim (x:xs) s = if isInfixOf s x
                    then read (drop (length s) x) :: Int
                    else getDim xs s

--return only the rows between the selected string s and ":end"
searchRows :: [String] -> String -> [String]
searchRows [] s = []
searchRows (x:xs) s | isInfixOf s x = endRows xs s
                      | otherwise = searchRows xs s

-- checks the end of the selected rows
endRows:: [String] -> String -> [String]
endRows [] s = []
endRows (x:xs) s | isInfixOf ":end" x = []
                    | otherwise = [x]  ++ endRows xs s
                                  


-- remove the brackets from the String. Example: ["(1,1)"] will be ["1,1"]
createTupleString :: String -> [String]
createTupleString [] = []
createTupleString s = [str] ++ createTupleString (drop ((length str)+2) s)
                      where str = tail (getStr s ')')

-- turns a tuplestring to tuples. Get ["1,1"] and return [(1,1)] 
createPosTuple :: [String] -> [Pos]
createPosTuple [] = []
createPosTuple (s:xs) = [(read x1 ::Int,read y1 ::Int)] ++ createPosTuple xs
                        where x1 = getStr s ','
                              y1 = drop (1+(length x1)) s

-- returns a string infront of the char              
getStr :: String -> Char -> String
getStr [] _ = []
getStr (x:xs) s | x == s = []
                | otherwise = [x] ++ getStr xs s







{-generateRoad gets the road strings and build the single roadcells between two points.
  The Cell contian the street id and the name of the street -}
generateRoad :: [String] -> [(Pos, Cell)]
generateRoad [] = []
generateRoad (x:xs) = fillRoadCell firstI second (buildRoadList pos) ++ generateRoad xs
  where first = getStr x ';'
        l1 = (length first) + 1
        firstI = read first :: Int
        second = getStr (drop l1 x) ';'
        l2 = (length second) + 1 + l1
        third = drop l2 x
        pos = createPosTuple (createTupleString third)                       
                     

--build one list for the way forth and one list for the way back
buildRoadList :: [Pos] -> [[Pos]]
buildRoadList pos  = if null pos
                        then []
                        else buildRoadPointToPoint (take 2 pos) : buildRoadList (drop 2 pos) 


--build a list of roadpoints for one direction of one road.
buildRoadPointToPoint :: [Pos] -> [Pos]
buildRoadPointToPoint ((x1,y1):(x2,y2):xs) = if (x1 <= x2)&&(y1<=y2)
                                               then [(x,y) | x <- [x1..x2], y <- [y1..y2]]
                                               else reverse [(x,y) | x <- [x2..x1], y <- [y2..y1]]


{-fill the road cell with informations. i will be the identifier, s will be the name
  of the street and the next roadpiece will be at the next position of the road.-}
fillRoadCell :: Int -> String -> [[Pos]] -> [(Pos, Cell)]
fillRoadCell i s [] = []
fillRoadCell i s (pos:xs) = findNextRoad i s pos ++ fillRoadCell i s xs


findNextRoad :: Int -> String -> [Pos] -> [(Pos, Cell)]
findNextRoad i s [] = []
findNextRoad i s (pos:xs) = (pos, Road i s next) : findNextRoad i s xs
                             where next = if null(xs)
                                             then []
                                             else [head xs]

-----------------------------------------------------------------------------------

{-divide the buildingstring into three pieces. First piece is the id of the building,
  second piece is the name of the houseowners and the last piece will be the location
  of the building.-}
generateBuilding :: [String] -> [(Pos, Cell)]
generateBuilding [] = []
generateBuilding (x:xs) = generateBuildingList pos firstI second ++ generateBuilding xs
                        where first = getStr x ';'
                              l1 = length (first) +1
                              firstI = read first :: Int
                              second = getStr (drop l1 x) ';'
                              l2 = l1 + (length second) + 1
                              third = drop l2 x
                              pos = createPosTuple (createTupleString third)

--builds the List for the buildings and fill the cell with informations
generateBuildingList :: [Pos] -> Int -> String -> [(Pos,Cell)]
generateBuildingList [] _ _ = []
generateBuildingList (x:xs) i s = [(z, Building i s) | z <- [x]]







{-generates the signallist. The relationships between the signals works with the 
  positions of the signals. Each signal knows other signals, who work as itself
  (list workWith) and who work different to itself (list workAgainst).
  Example: If the signal it green, then the signals in workWith are also green. 
  But the singals in workAgainst are red!-}
generateSignals :: [String] ->[(Pos, Cell)]
generateSignals s = buildRelationships listCell listCell
                        where listCell = genListCell s


{-build a list of cells without relationships. stingListBeforeChar returns a list
  which divided a string into substrings at the point of the ';' char. Each new string
  will be reworked to an integer or to a position. Then a signal cell will be
  createt with only its id. The result of the function will be a position, a cell and 
  two lists with id's. First list comprised the workWith signalid's and the second list 
  comprised the workAgainst signalid's.-}
genListCell :: [String] -> [(Pos, Cell, [Int], [Int])]
genListCell [] = []
genListCell (x:xs) = [(head pos, (Signal id status step step [] []), wW, wA)] ++ genListCell xs
                            where a = stringListBeforeChar x ';'
                                  id = read (head a) :: Int
                                  pos = createPosTuple (createTupleString (a!!1))
                                  status = "green" == (a!!2)
                                  step = read (a!!3) :: Int
                                  wW = getIntRel (a!!4)
                                  wA = getIntRel (a!!5)


--makes the relationship id's from a string to an integer list
getIntRel :: String -> [Int]
getIntRel s = map readI (stringListBeforeChar (tail (makeString s ']')) ',')

--returns an integer from a String
readI :: String -> Int
readI s = read s :: Int



{-builds the realationships between the signals. It will get two lists, because the 
  returned cell needs the coordinates of the other cells. First list is the source
  for the returning cell and the second list is to find the coordinates of the cells
  with the right id. The id's are stored in the two integerlists. Firs integer-list is 
  the workWith-list and the second integer-list is the workAgainst-list-}
buildRelationships :: [(Pos, Cell , [Int], [Int])] -> [(Pos, Cell , [Int], [Int])] -> [(Pos,Cell)]
buildRelationships [] l = []
buildRelationships ((xy,Signal id st sA sR workWith against, wId, aId):xs) l = [(xy, Signal {ident=id, status = st, stepToWait=sA, remainingSteps= sR, workWith = with, against = workAgainst})] ++ buildRelationships xs l
                        where with = map (findPartner l) wId
                              workAgainst = map (findPartner l) aId

--find the partner cell with the id in the second list named ids and 
--returns the position coordinates
findPartner :: [(Pos, Cell, [Int], [Int])] -> Int -> Pos
findPartner ((pos, (Signal idd st sA sR wW wA), i1, i2):xs) ids | idd == ids = pos
                                                                | otherwise = findPartner xs ids








{-build the car list and the path of the cars. It extracts the id, the startposition
  and the endposition from the stringlist. If there was only an id of a house, then 
  it will be searching the nearest road of the house. This roadpiece will be the start
  position of the car. Then the path of the car will be calculated
  with the function makePath. After all the cell will be created-}
generateCarList :: [String] -> [(Pos, Cell)] -> [(Pos, Cell)] -> [(Pos, Cell)]
generateCarList [] _ _= []
generateCarList (x:xs) building roads =
                 [(pos, Car id end [])] ++ generateCarList xs building roads
                           where a = stringListBeforeChar x ';'
                                 id = read (head a) ::Int
                                 pos = findPos building roads (a!!1)
                                 end = findPos building roads (a!!2)
                                 

{- Extract the id of the house or the starting positon tuple-}
findPos ::  [(Pos, Cell)] -> [(Pos, Cell)] -> String -> Pos
findPos build street pos = if head pos=='H'
                            then findStreetForBuilding build street (readI(tail pos))
                            else head(createPosTuple (createTupleString pos))



{- After the id of the house was found, it will search above, below, left and right
   of the house for a road. First roadpiece will be used as startingpoint of the car.-}
findStreetForBuilding :: [(Pos, Cell)] -> [(Pos, Cell)] -> Int -> Pos
findStreetForBuilding build street id = head positions
    where buildingPos = [posB | (posB, _)<-(filter (\(_,Building identB nameB) -> identB ==id) build)]
          positions = [posS | (posS, _) <- street, (x,y) <- buildingPos,
                       posS==(x,y-1)|| posS==(x-1,y) || posS==(x+1,y) ||
                       posS==(x,y+1)]



--------------------------------------------------------------------------------





{-now the city will be created. The first list is like a two dimension array
  each row is a seperate list in the list. That guaranteed the x and y positions.
  And the second list is only a normal list, which comprised the cars and the signals
  with the current positions. -}
buildCity :: Int -> Int -> [(Pos,Cell)] -> [(Pos,Cell)] -> City
buildCity w h streetBuild sigCar = City { width            = w,
                                          height           = h,
                                          static           = sB,
                                          dynamic          = sigCar}
    where sB = buildList w h streetBuild


--the rows jointed together
buildList :: Int -> Int -> [(Pos,Cell)] -> [[Cell]]
buildList w h cl = [buildListRow w i cl | i <- [1..h]]

--each row a seperate list
buildListRow :: Int -> Int -> [(Pos,Cell)] -> [Cell]
buildListRow w h cl = [returnCellXY x h cl | x <- [1..w]]



-- returns the cell at x,y. But when nothnig is there, an empty cell will be returned.
-- it extracts also the buildings and the roads.
returnCellXY :: Int -> Int -> [(Pos,Cell)] -> Cell
returnCellXY x y list = if length cellList ==0
                           then Empty {} 
                           else case (head cellList) of 
                                 { (pos, Building ident name)     -> Building ident name;
                                   (pos, Road ident name nextRoad)-> buildDeadEndRoadJunction cellList list
                                 }
                        where cellList = filter (\((x1,y1), cell) -> x1==x && y1 == y) list


-- this one generates all deadends and roadjunctions. If only one cell is in the list, 
-- then this one will be returned after checking for the next road piece.
buildDeadEndRoadJunction :: [(Pos,Cell)] -> [(Pos,Cell)] -> Cell
buildDeadEndRoadJunction cellList list = if length cellList == 1 
                                            then buildDeadEnd list (head cellList)
                                            else roadJunction (map (buildDeadEnd list) cellList)


-- the function search for the next road piece. If nothing was found, then it searches for
-- the nearest pice of the same road!
buildDeadEnd :: [(Pos,Cell)] -> (Pos,Cell) -> Cell
buildDeadEnd list ((x,y), Road id name roadPath) = 
        if null roadPath
           then Road id name [next]
           else Road id name roadPath
        where nearestStreetCells = filter (\((x1,y1),_) -> (x1==x-1 || x1==x+1)&& y1==y 
                                                    || x1==x&&(y1==y-1 || y1==y+1)) list
              onlyThisStreetCells = filter (\(_, Road ident _ _) -> ident == id) nearestStreetCells
              notBeforeThisCell = filter (\(_, Road _ _ path) -> (head path) /= (x,y)) onlyThisStreetCells
              next = (\(pos,_) -> pos) (head notBeforeThisCell)  


{- build the roadjunctions. One junction will be four cells, and each cell knows 2 
   directions. The next cell in direction and the left or right cell, addicted by the
   direction of the cell on the right/left.-}
roadJunction :: [Cell] -> Cell
roadJunction roads = Road id name roadPath
                     where id = (\(Road ident name nextRoad) -> ident) (head roads)
                           name = (\(Road ident name nextRoad) -> name) (head roads)
                           roadPath = nub (map (\(Road ident name nextRoad) -> head nextRoad) roads)
