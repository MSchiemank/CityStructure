import Data.List
--Datatype setting

data Cell = Road    { ident   :: String,
                      name    :: String}
          | Building{ ident   :: String,
                      name    :: String}
          | Signal  { ident   :: String,
                      workWith :: String,
                      against  :: [String]}
          | Car     { ident   :: String}
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
                   roads = buildSegment s "Roads:"-}

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

--checks the first element of the roadlists
buildSegment :: [String] -> String -> [((Int, Int), [String])]
buildSegment [] s = []
buildSegment (x:xs) s | isInfixOf s x = endSegment xs s
                      | otherwise = buildSegment xs s

-- checks the end of the road list
endSegment:: [String] -> String -> [((Int, Int), [String])]
endSegment [] s = []
endSegment (x:xs) s | isInfixOf "end" x = []
                    | otherwise = if s == "Roads:" 
                                 then generateRoad x ++ endSegment xs s
                                 else  if s == "Buildings:"
                                        then generateBuilding x ++ endSegment xs s
                                        else endSegment [] s

generateRoad :: String -> [((Int, Int), [String])]
generateRoad [] = []
generateRoad x = buildRoadList pos first second
                 where first = getStr x ';'
                       second = getStr (drop ((length first)+1) x) ';'
                       third = drop((length first)+(length second)+2) x
                       pos = createPosTuple (createTupleString third)
                    

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

                           
--builds a full list of roadpieces                        
buildRoadList :: [(Int,Int)] -> String -> String -> [((Int, Int), [String])]
buildRoadList [] _ _ = []
buildRoadList ((x1,y1):(x2,y2):zs) s1 s2 = [((x,y),s) | x <- [x1..x2], y <- [y1..y2], s <- [[s1]++[s2]]] ++ buildRoadList zs s1 s2


--generates the buildinglist
--buildBuilding :: [String] -> [((Int, Int), [String])]
generateBuilding s = generateBuildingList pos first second
                        where first = getStr s ';'
                              second = getStr (drop ((length first)+1) s) ';'
                              third = drop ((length first)+(length second)+2) s
                              pos = createPosTuple (createTupleString third)

--builds the List for the buildings
generateBuildingList :: [(Int,Int)] -> String -> String -> [((Int,Int),[String])]
generateBuildingList [] _ _ = []
generateBuildingList (x:xs) s1 s2 = [(z, s) | z <- [x], s <- [[s1]++[s2]]] ++ generateBuildingList xs s1 s2

{-main :: IO()
main = run (readFile "city")


run :: IO String ->IO()
run file = do let string = doInputString file
              putStr string-}
