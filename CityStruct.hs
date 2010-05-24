import Data.List
import Char



--Starting the programm with the main function
main :: IO()
main = do file <- readFile "city"
          run (parse (doInputString file))


run :: City -> IO()
run city = do printCity city




------------------------------------------------------

--Datatype setting
data Cell = Road    { ident    :: Integer,
                      name     :: String}
          | Building{ ident    :: Integer,
                      name     :: String}
          | Signal  { ident    :: Integer,
                      workWith :: [Pos],
                      against  :: [Pos]}
          | Car     { ident    :: Integer,
                      path     :: [Pos]}
          | Empty {}
     deriving (Eq, Show)

type Pos = (Int, Int)
data City = City { width              :: Int,
                   height             :: Int,
                   static             :: [[Cell]],
                   dynamic            :: [[Cell]]}
     deriving (Eq, Show)

--returns the contents of the structure
getCityStatic :: City -> [[Cell]]
getCityStatic (City width height static dynamic) = static

getCityDynamic :: City -> [[Cell]]
getCityDynamic (City width height static dynamic) = dynamic

getCityWidth :: City -> Int
getCityWidth (City width height static dynamic) = width

getCityHeight :: City -> Int
getCityHeight (City width height static dynamic) = height


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
printCell city pos = if dyn /= Empty
                        then cellToChar dyn
                        else cellToChar stat
                       where dyn = getCell (getCityDynamic city) pos
                             stat = getCell (getCityStatic city) pos
    

getCell :: [[Cell]] -> (Int,Int) -> Cell
getCell cell (x,y) = (cell!!(y-1))!!(x-1)

cellToChar :: Cell -> Char
cellToChar cell = 
        case cell of
          { (Road ident name)               -> 'R';
            (Building ident name)           -> 'H';
            (Signal ident workWith against) -> 'S';
            (Car ident path)                -> 'C';
            Empty                           -> ' '
          }




{-------------------------------------------------------------
doInputString get a String from the function readFile and makes one String for each row.
It will also remove the space between the words/chars, it remove the commentlines and the empty lines of the String.

rmNullString remove the empty lines, rmSpace remove the spaces anf rmCommentLine remove the commandlines. stringListBeforeChar builds the String for each row and makeString will help to find the newline-char. -}

--creates a String without the comments and the clear lines
doInputString :: String -> [String]
doInputString file = do let string = stringListBeforeChar file '\n'
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
                   carCells = generateCarList (searchRows s ":Cars")

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
generateRoad (x:xs) = fillRoadCell (buildRoadList pos firstI second) ++ generateRoad xs
  where first = getStr x ';'
        l1 = (length first) + 1
        firstI = read first :: Integer
        second = getStr (drop l1 x) ';'
        l2 = (length second) + 1 + l1
        third = drop l2 x
        pos = createPosTuple (createTupleString third)
                     
                       
--build a full list of roadpoints for one direction of one road.
buildRoadList :: [Pos] -> Integer -> String -> [(Pos, Integer,String)]
buildRoadList [] i s = []
buildRoadList ((x1,y1):(x2,y2):zs) i s = if (x1 <= x2)&&(y1<=y2)
                                            then [((x,y),i,s) | x <- [x1..x2], y <- [y1..y2]] ++ buildRoadList zs i s
                                            else [((x,y),i,s) | x <- [x2..x1], y <- [y2..y1]] ++ buildRoadList zs i s

--fill the road cell with informations
fillRoadCell :: [(Pos,Integer, String)] -> [(Pos, Cell)]
fillRoadCell [] = []
fillRoadCell ((pos,i,s):xs) = [(pos, c)] ++ fillRoadCell xs
                                where c = Road i s






{-divide the buildingstring into three pieces. First piece is the id of the building,
  second piece is the name of the houseowners and the last piece will be the location
  of the building.-}
generateBuilding :: [String] -> [(Pos, Cell)]
generateBuilding [] = []
generateBuilding (x:xs) = generateBuildingList pos firstI second ++ generateBuilding xs
                        where first = getStr x ';'
                              l1 = length (first) +1
                              firstI = read first :: Integer
                              second = getStr (drop l1 x) ';'
                              l2 = l1 + (length second) + 1
                              third = drop l2 x
                              pos = createPosTuple (createTupleString third)

--builds the List for the buildings and fill the cell with informations
generateBuildingList :: [Pos] -> Integer -> String -> [(Pos,Cell)]
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
genListCell :: [String] -> [(Pos, Cell, [Integer], [Integer])]
genListCell [] = []
genListCell (x:xs) = [(head pos, (Signal id [] []), wW, wA)] ++ genListCell xs
                            where a = stringListBeforeChar x ';'
                                  id = read (head a) :: Integer
                                  pos = createPosTuple (createTupleString (a!!1))
                                  wW = getIntRel (a!!2)
                                  wA = getIntRel (a!!3)

--makes the relationship id's from a string to an integer list
getIntRel :: String -> [Integer]
getIntRel s = map readI (stringListBeforeChar (tail (makeString s ']')) ',')

--returns an integer from a String
readI :: String -> Integer
readI s = read s :: Integer



{-builds the realationships between the signals. It will get two lists, because the 
  returned cell needs the coordinates of the other cells. First list is the source
  for the returning cell and the second list is to find the coordinates of the cells
  with the right id. The id's are stored in the two integerlists. Firs integer-list is 
  the workWith-list and the second integer-list is the workAgainst-list-}
buildRelationships :: [(Pos, Cell , [Integer], [Integer])] -> [(Pos, Cell , [Integer], [Integer])] -> [(Pos,Cell)]
buildRelationships [] l = []
buildRelationships ((xy,Signal id workWith against, wId, aId):xs) l = [(xy, Signal {ident=id, workWith = with, against = workAgainst})] ++ buildRelationships xs l
                        where with = map (findPartner l) wId
                              workAgainst = map (findPartner l) aId

--find the partner cell with the id in the second list named ids and 
--returns the position coordinates
findPartner :: [(Pos, Cell, [Integer], [Integer])] -> Integer -> Pos
findPartner ((pos, (Signal idd wW wA), i1, i2):xs) ids | idd == ids = pos
                                                       | otherwise = findPartner xs ids








{-build the car list and the path of the cars. It extracts the id, the startposition
  and the endposition from the stringlist. Then the path of the car will be calculated
  with the function makePath. After all the cell will be created-}
generateCarList :: [String] -> [(Pos, Cell)]
generateCarList [] = []
generateCarList (x:xs) = [(head path, Car id path)] ++ generateCarList xs
                           where a = stringListBeforeChar x ';'
                                 id = read (head a) ::Integer
                                 pos = createPosTuple (createTupleString (a!!1))
                                 end = createPosTuple (createTupleString (a!!2))
                                 path = makePath pos end

--first makePath function, which goes from point a to b in a line
makePath :: [Pos] -> [Pos] -> [Pos]
makePath ((x1,y1):xs1) ((x2,y2):xs2) = [(x,y) | x <- [x1..x2] , y <- [y1..y2]]





{-now the city will be created. The two lists ar like a two dimension array
  each row is a seperate list in the list. That guaranteed the x and y positions -}
buildCity :: Int -> Int -> [(Pos,Cell)] -> [(Pos,Cell)] -> City
buildCity w h streetBuild sigCar = City { width            = w,
                                          height           = h,
                                          static           = sB,
                                          dynamic          = cS}
    where sB = buildList w h streetBuild
          cS = buildList w h sigCar

--the rows jointed together
buildList :: Int -> Int -> [(Pos,Cell)] -> [[Cell]]
buildList w h cl = [buildListRow w i cl | i <- [1..h]]

--each row a seperate list
buildListRow :: Int -> Int -> [(Pos,Cell)] -> [Cell]
buildListRow w h cl = [returnCellXY x h cl | x <- [1..w]]

returnCellXY :: Int -> Int -> [(Pos,Cell)] -> Cell
returnCellXY x y [] = Empty {}
returnCellXY x y (((x1,y1),c):ls) = if x1 == x && y1 == y
                                     then c
                                     else returnCellXY x y ls
