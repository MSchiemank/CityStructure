import Data.List
--Datatype setting

data Cell = Road    { ident   :: Integer,
                      name    :: String}
          | Building{ ident   :: Integer,
                      name    :: String}
          | Signal  { ident   :: Integer,
                      workWith :: [Cell],
                      against  :: [Cell]}
          | Car     { ident   :: Integer}
     deriving (Eq, Show)


--myCell = Road { ident = "R4", name = "Berliner"}


--creates a String without the comments and the clear lines
doInputString :: String -> [String]
doInputString file = do let string = makeAll file
                        rmSpace (remCommentLine (rmNullString string))


-- remove the clear lines
rmNullString :: [String] -> [String]
rmNullString [] = []
rmNullString (x:xs) | x == "" = rmNullString xs
                    | otherwise = [x] ++ rmNullString xs

-- remove the space in the strings
rmSpace :: [String] -> [String]
rmSpace [] = []
rmSpace (x:xs) = [[s | s <- x, s /= ' ']] ++ rmSpace xs

-- remove the comment lines
remCommentLine :: [String] -> [String]
remCommentLine [] = []
remCommentLine (x:xs) | head x /= '-' = [x] ++ remCommentLine xs
                      | otherwise = remCommentLine xs

-- builds a string with each listelement is one row of the file
makeAll :: String -> [String]
makeAll [] = []
makeAll file = [row] ++ makeAll (drop ((length row)+1)file)
            where row = makeRow file
              
-- selects the row of the file
makeRow :: [Char] -> [Char]
makeRow (x:xs) | x == '\n' = []
               | otherwise = [x] ++ makeRow xs


--Parser is todo
--parse :: [String] -> [[Cell]]
{-parse s = do 
             where hight = getHight s
                   width = getWidth s
                   roads = buildSegment s "Roads:"
                   buildings = buildSegment s "Buildings:"
                   signals = buildSegment s "Signals:"-}

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

--checks the first element of the selected lists
buildSegment :: [String] -> String -> [((Int, Int), Cell)]
buildSegment [] s = []
buildSegment (x:xs) s | isInfixOf s x = endSegment xs s
                      | otherwise = buildSegment xs s

-- checks the end of the selected list
endSegment:: [String] -> String -> [((Int, Int), Cell)]
endSegment [] s = []
endSegment (x:xs) s | isInfixOf "end" x = []
                    | otherwise = if s == "Roads:" 
                                  then generateRoad x ++ endSegment xs s
                                  else if s == "Buildings:"
                                        then generateBuilding x ++ endSegment xs s
                                        else []


--divide the String into 3 pices
generateRoad :: String -> [((Int, Int), Cell)]
generateRoad [] = []
generateRoad x = fillRoadCell (buildRoadList pos firstI second)
                 where first = getStr x ';'
                       l1 = (length first) + 1
                       firstI = read first :: Integer
                       second = getStr (drop l1 x) ';'
                       l2 = (length second) + 1 + l1
                       third = drop l2 x
                       pos = createPosTuple (createTupleString third)
                    
                           
--builds a full list of roadpieces                        
buildRoadList :: [(Int,Int)] -> Integer -> String -> [((Int, Int), Integer,String)]
buildRoadList [] _ _ = []
buildRoadList ((x1,y1):(x2,y2):zs) i s = [((x,y),i,s) | x <- [x1..x2], y <- [y1..y2]] ++ buildRoadList zs i s

--fill the road cell with informations
fillRoadCell :: [((Int,Int),Integer, String)] -> [((Int,Int), Cell)]
fillRoadCell [] = []
fillRoadCell ((pos,i,s):xs) = [(pos, c)] ++ fillRoadCell xs
                                where c = Road i s



--divide the buildingstring into three pices
generateBuilding :: String -> [((Int, Int), Cell)]
generateBuilding s = generateBuildingList pos firstI second
                        where first = getStr s ';'
                              l1 = length (first) +1
                              firstI = read first :: Integer
                              second = getStr (drop l1 s) ';'
                              l2 = l1 + (length second) + 1
                              third = drop l2 s
                              pos = createPosTuple (createTupleString third)

--builds the List for the buildings
generateBuildingList :: [(Int,Int)] -> Integer -> String -> [((Int,Int),Cell)]
generateBuildingList [] _ _ = []
generateBuildingList (x:xs) i s = [(z, Building i s) | z <- [x]]


{--generates the signallist
generateSignals :: String ->[((Int,Int), Cell)]
generateSignals s = generateSignalList pos firstI rel antiRel
                        where first = getStr s ';'
                              i1 = (length first) + 1
                              firstI = read first :: Integer
                              second = getStr (drop i1 s) ';'
                              i2 = i1 + 1 + (length second)
                              pos = createPosTuple (createTupleString second)
                              rel = getStr (drop i2 s) ';'
                              i3 = i2 + 1 + (length rel)
                              antiRel = drop i3 s

--generateSignalList :: [(Int,Int)] -> String -> String -> String -> [((Int,Int), [String])]
generateSignalList (x:xs) s1 s2 s3 = (x,Signal s1 s2 s3)-}


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
createPosTuple :: [String] -> [(Int,Int)]
createPosTuple [] = []
createPosTuple (s:xs) = [(read x1 ::Int,read y1 ::Int)] ++ createPosTuple xs
                        where x1 = getStr s ','
                              y1 = drop (1+(length x1)) s


{-main :: IO()
main = run (readFile "city")


run :: IO String ->IO()
run file = do let string = doInputString file
              putStr string-}
