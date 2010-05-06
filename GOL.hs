-- Dimensions of the Field
hight :: Int
hight = 20

width :: Int
width = 20

-- Marker
mark = '0'
unmark = '.'

-- definition of the structure for the field
type Pos =(Int, Int)
type Cel = (Pos, Char)
type Field =[Cel]

-- The startingposition of the first lifes on the field
startPos :: [(Int,Int)]
-- startPos = [(3,4),(4,2),(4,3),(2,3),(4,4)]

-- the f-Pentomino
startPos = [(9,10),(9,11),(10,9),(10,10),(11,10)]


-- we are building the field. It's a list, thats like a x,y Array.
doField :: [(Int,Int)] -> Int -> Int -> [((Int,Int),Char)]
doField pos x y | y == width = [((x,y),(fillPos pos x y))] ++ doField pos (x+1) 1  -- a new row
                | x > hight = []                                                   -- function done
                | otherwise = [((x,y),(fillPos pos x y))] ++ doField pos x (y+1)   -- normaly

-- will fill the second tuple with life (0) or no life ( )
fillPos :: (Eq a, Eq b) =>[(a,b)] -> a -> b -> Char
fillPos [] _ _ = unmark 
fillPos (x:xs) y z = if x == (y,z)
			then mark
			else fillPos xs y z


-- returns the element from the x,y Cell of the field-Array 
gCellC:: [((Int,Int),Char)]-> (Int,Int) -> Char
gCellC f t = snd (f!!(((fst t)-1)*width+((snd t)-1)))


-- Output on command line for the whole field
printAll :: [((Int,Int),Char)] -> Int -> IO ()
printAll field c = do putStr (printF field 1 1 [])
                      putStrLn ("Cycle: "++show c)
                      

-- generates an outputstring with a newline after a row
printF :: [((Int,Int),Char)] -> Int -> Int -> String -> String
printF field x y out | x > hight = out 
                     | y == width = out ++ [gCellC field (x,y)] ++ ['\n'] ++ printF field (x+1) 1 out
                     | otherwise = out ++ [(gCellC field (x,y))] ++ printF field x (y+1) out


-- creating the next living list
nextStep :: [((Int,Int),Char)] -> [(Int,Int)] -> [(Int,Int)]
nextStep field l = (survive field l)++(born field field)

-- for every item in the list will be shown, if there are 2 or 3 neighbours
survive :: [((Int,Int),Char)] -> [(Int,Int)] -> [(Int,Int)]
survive f [] = []
survive f (x:xs) = do let cell = length (checkCell f x)
                      if  cell >=2 && cell <=3 
                          then [x] ++ (survive f xs)
                          else survive f xs

-- the same as survive, but it will be look in every cell and a new child will be born 
-- if there are 2 neighbours
born :: [((Int,Int),Char)] -> [((Int,Int),Char)] -> [(Int,Int)]
born f [] = []
born f ((a,b):xs) = do let cell = length (checkCell f a)
                       if cell == 3 
                          then [a] ++ born f xs
                          else born f xs

-- checks the eight neighbours of the cell
checkCell :: [((Int,Int),Char)] -> (Int,Int) ->[Char]
checkCell f (x,y) = rmSpace (cCell f [(x-1,y-1),(x-1,y),(x-1,y+1),
                                      (x,y-1),(x,y+1),
                                      (x+1,y-1),(x+1,y),(x+1,y+1)])
-- remove the space in the list
rmSpace :: [Char] -> [Char]
rmSpace [] = []
rmSpace (x:xs) | x == mark = [x]++rmSpace xs
               | otherwise = rmSpace xs


-- returns the list of the neighbour. 
-- illegal rows/columns will be filtered out
cCell :: [((Int,Int),Char)] -> [(Int,Int)] -> [Char]
cCell f [] = []
cCell f ((a,b):xs) = if a < 1 || a > hight
                      then cCell f xs
                      else if b < 1 || b > width
                              then cCell f xs
                              else [gCellC f (a,b)] ++ cCell f xs

--wait function
wait :: Int -> IO()
wait t = if t > 0 
            then do return()
                    wait (t-1)
            else return ()


-- startfunction
main :: IO()  
main = mainStart startPos 1


-- start and working function
mainStart :: [(Int,Int)] -> Int -> IO()
mainStart sl c = do let arr = doField sl 1 1
                    printAll arr c
                    let next = nextStep arr sl
                    wait(100000)
                    mainStart next (c+1)

