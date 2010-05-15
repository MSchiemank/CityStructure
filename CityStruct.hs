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
                   width = getWidth s-}

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
buildRoad :: [String] -> [((Int, Int), [String])]
buildRoad [] = []
buildRoad (x:xs) | isInfixOf "Roads:" x = endRoad xs
                 | otherwise = buildRoad xs

-- checks the end of the road list
endRoad :: [String] -> [((Int, Int), [String])]
endRoad [] = []
endRoad (x:xs) | isInfixOf "end" x = []
               | otherwise = generateRoad x ++ endRoad xs

--generateRoad :: [String] -> (Int, Int, [String])
generateRoad [] = []
generateRoad x = buildRoadList pos first second
                 where first = getStr x ';'
                       second = getStr (drop ((length first)+1) x) ';'
                       third = getStr (drop((length first)+(length second)+2) x) ';'
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

                           
--builds a full list of the roadpieces                        
buildRoadList :: [(Int,Int)] -> String -> String -> [((Int, Int), [String])]
buildRoadList [] _ _ = []
buildRoadList ((x1,y1):(x2,y2):zs) s1 s2 = [((x,y),s) | x <- [x1..x2], y <- [y1..y2], s <- [[s1]++[s2]]] ++ buildRoadList zs s1 s2

{-main :: IO()
main = run (readFile "city")


run :: IO String ->IO()
run file = do let string = doInputString file
              putStr string-}
