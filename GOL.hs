--import Array
hight :: Int
hight = 10

width :: Int
width = 20

type Pos =(Int, Int)
type Cel = (Pos, Char)
type Field =[Cel]

alife :: [(Int,Int)]
alife = [(2,3),(3,4),(4,2),(4,3),(4,4)]

doField :: [(Int,Int)] -> Int -> Int -> [((Int,Int),Char)]
doField pos x y | y == width = [((x,y),(fillPos alife x y))] ++ doField pos (x+1) 1
                | x > hight = []
                | otherwise = [((x,y),(fillPos alife x y))] ++ doField pos x (y+1)

fillPos :: (Eq a, Eq b) =>[(a,b)] -> a -> b -> Char
fillPos [] _ _ = ' ' 
fillPos (x:xs) y z = if x == (y,z)
			then '0'
			else fillPos xs y z

gchar:: (a,b)-> b
gchar (a,b) = b

gCell:: [((Int,Int),Char)]-> Int -> Int -> Char
gCell f x y = gchar (f!!((x-1)*width+(y-1)))

printAll :: IO ()
printAll = putStr (printF (doField alife 1 1) 1 1 [])

printF :: [((Int,Int),Char)] -> Int -> Int -> String -> String
printF field x y out | x > hight = out ++ ['\n']
                     | y == width = out ++ [gCell field x y] ++ ['\n'] ++ printF field (x+1) 1 out
                     | otherwise = out ++ [(gCell field x y)] ++ printF field x (y+1) out

{-nextStep
nextStep
main :: IO()  

main = mainStart alive

mainStart :: [((Int,Int),Char)] -> IO()
mainStart al = do -}

