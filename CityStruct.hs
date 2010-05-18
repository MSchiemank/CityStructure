import Data.List
import Char
--Datatype setting

data Cell = Road    { ident   :: Integer,
                      name    :: String}
          | Building{ ident   :: Integer,
                      name    :: String}
          | Signal  { ident   :: Integer,
                      workWith :: [Pos],
                      against  :: [Pos]}
          | Car     { ident   :: Integer,
                      path    :: [Pos]}
     deriving (Eq, Show)

type Pos = (Int, Int)
data City = City { width  :: Integer,
                   height :: Integer}

--creates a String without the comments and the clear lines
doInputString :: String -> [String]
doInputString file = do let string = makeAll file '\n'
                        rmSpace (remCommentLine (rmNullString string))


-- remove the clear lines
rmNullString :: [String] -> [String]
rmNullString lines = filter (\x -> length x > 0) lines

-- remove the space in the strings
rmSpace :: [String] -> [String]
rmSpace [] = []
rmSpace (x:xs) =[filter(\s -> not (isSpace s)) x] ++ rmSpace xs

-- remove the comment lines
remCommentLine :: [String] -> [String]
remCommentLine lines = filter (\x -> (head x) /='-') lines

-- builds a string with each listelement is one row of the file
makeAll :: String -> Char -> [String]
makeAll [] c = []
makeAll file c = [row] ++ makeAll (drop ((length row)+1)file) c
                 where row = makeString file c
              
-- selects the row of the file
makeString :: [Char] -> Char -> [Char]
makeString [] c = []
makeString (x:xs) c | x == c = []
                    | otherwise = [x] ++ makeString xs c


--Parser is todo
--parse :: [String] -> [[Cell]]
{-parse s = do 
             where City {width = getWidth s,
                         hight = getHight s}
                   roadCells = generateRoad (searchX s ":Roads")
                   buildingCells = generateBuilding (searchX s ":Buildings")
                   signalCells = generateSignal (searchX s ":Signals")
                   carList = generateCarList (serchX s ":Cars")-}

-- returns the width of the city
getWidth :: [String] -> Int
getWidth [] = 1
getWidth (x:xs) = if isInfixOf "width=" x
                    then read (drop (length "width=") x) :: Int
                    else getWidth xs

-- returns the hight of the city
getHight :: [String] -> Int
getHight [] = 1
getHight (x:xs) = if isInfixOf "hight=" x
                    then read (drop (length "hight=") x) :: Int
                    else getHight xs

--checks the first element of the selected list
searchX :: [String] -> String -> [String]
searchX [] s = []
searchX (x:xs) s | isInfixOf s x = endX xs s
                      | otherwise = searchX xs s

-- checks the end of the selected list
endX:: [String] -> String -> [String]
endX [] s = []
endX (x:xs) s | isInfixOf ":end" x = []
                    | otherwise = [x]  ++ endX xs s
                                  

--divide the String into 3 pices
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
                    
                           
--builds a full list of roadpieces                        
buildRoadList :: [Pos] -> Integer -> String -> [(Pos, Integer,String)]
buildRoadList [] _ _ = []
buildRoadList ((x1,y1):(x2,y2):zs) i s = [((x,y),i,s) | x <- [x1..x2], y <- [y1..y2]] ++ buildRoadList zs i s

--fill the road cell with informations
fillRoadCell :: [(Pos,Integer, String)] -> [(Pos, Cell)]
fillRoadCell [] = []
fillRoadCell ((pos,i,s):xs) = [(pos, c)] ++ fillRoadCell xs
                                where c = Road i s



--divide the buildingstring into three pices
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

--builds the List for the buildings
generateBuildingList :: [Pos] -> Integer -> String -> [(Pos,Cell)]
generateBuildingList [] _ _ = []
generateBuildingList (x:xs) i s = [(z, Building i s) | z <- [x]]


--generates the signallist
generateSignals :: [String] ->[(Pos, Cell)]
generateSignals s = buildRelationships listCell listCell
                        where listCell = genListCell s


--builds the realationships between the signals
buildRelationships :: [(Pos, Cell , [Integer], [Integer])] -> [(Pos, Cell , [Integer], [Integer])] -> [(Pos,Cell)]
buildRelationships [] l = []
buildRelationships ((xy,Signal id workWith against, wId, aId):xs) l = [(xy, Signal {ident=id, workWith = with, against = workAgainst})] ++ buildRelationships xs l
                        where with = map (findPartner l) wId
                              workAgainst = map (findPartner l) aId

--find the partner cell with the id in ids and returns the Position
findPartner :: [(Pos, Cell, [Integer], [Integer])] -> Integer -> Pos
findPartner ((pos, (Signal idd wW wA), i1, i2):xs) ids | idd == ids = pos
                                                       | otherwise = findPartner xs ids


--build a list of cells without relationships
genListCell :: [String] -> [(Pos, Cell, [Integer], [Integer])]
genListCell [] = []
genListCell (x:xs) = [(head pos, (Signal id [] []), wW, wA)] ++ genListCell xs
                            where a = makeAll x ';'
                                  id = read (head a) :: Integer
                                  pos = createPosTuple (createTupleString (a!!1))
                                  wW = getIntRel (a!!2)
                                  wA = getIntRel (a!!3)


--makes the relationship from a string to an integer list
getIntRel :: String -> [Integer]
getIntRel s = map readI (makeAll (tail (makeString s ']')) ',')


--build the car list and the path of the cars
generateCarList :: [String] -> [(Pos, Cell)]
generateCarList [] = []
generateCarList (x:xs) = [(head path, Car id path)] ++ generateCarList xs
                           where a = makeAll x ';'
                                 id = readI (head a)
                                 pos = createPosTuple (createTupleString (a!!1))
                                 end = createPosTuple (createTupleString (a!!2))
                                 path = makePath pos end

--first proprietary makePath function
makePath :: [Pos] -> [Pos] -> [Pos]
makePath ((x1,y1):xs1) ((x2,y2):xs2) = [(x,y) | x <- [x1..x2] , y <- [y1..y2]]


--returns an integer from a String
readI :: String -> Integer
readI s = read s :: Integer


-- returns a string infront of the char              
getStr :: String -> Char -> String
getStr [] _ = []
getStr (x:xs) s | x == s = []
                | otherwise = [x] ++ getStr xs s

-- divide the last string in tuplestrings
createTupleString :: String -> [String]
createTupleString [] = []
createTupleString s = [str] ++ createTupleString (drop ((length str)+2) s)
                      where str = tail (getStr s ')')

-- turns a tuplestring to tuples
createPosTuple :: [String] -> [Pos]
createPosTuple [] = []
createPosTuple (s:xs) = [(read x1 ::Int,read y1 ::Int)] ++ createPosTuple xs
                        where x1 = getStr s ','
                              y1 = drop (1+(length x1)) s


{-main :: IO()
main = run (readFile "city")


run :: IO String ->IO()
run file = do let string = doInputString file
              putStr string-}
