type Coord = (Int, Int)
data Orientation = H | V deriving(Show, Eq)
type Vehicle = (Orientation, Coord, Int) --Orientación, Coordenada inicial, Longitud
type Board = [Vehicle]

positions::Vehicle->[Coord]
positions (_,_,0) = []
positions (o,(i,j),l)
  | o == H    = (i,j):positions (o,(i, j+1),l-1)
  | otherwise = (i,j):positions (o,(i+1, j),l-1)

verify::Vehicle->Board->Bool
verify _ [] = True
verify v (x:xs)
  | not (null [(i,j) | (i,j)<-positions v, (i<0 || i>5) || (j<0 || j>5)]) = False
  | null [y | y<-positions v, elem y (positions x)] = verify v xs
  | otherwise = False
-- Por cada posicion de v se comprueba si coincide con alguna de x

-- Pregunta 1
initialBoard :: [Vehicle] -> Board
initialBoard [] = []
initialBoard [x] = [x]
initialBoard (x:xs)
  | verify x xs = if null (initialBoard xs) then [] else x:xs
  | otherwise = []

--Pregunta 2
isValidMove :: Board -> Int -> Int -> Bool
isValidMove b i n= not (null (initialBoard (iteRate b i n)))

iteRate:: [Vehicle] -> Int -> Int -> [Vehicle] 
iteRate (x:xs) 0 n = (modificar x n):xs
iteRate (x:xs) i n
  |i >0 = x:iteRate xs (i-1) n
  
modificar :: Vehicle -> Int -> Vehicle
modificar (o,(i,j),l) n
  | o == H = (o,(i,j+n),l)
  | otherwise = (o,(i+n,j),l)
  
--Pregunta 3
moveVehicle :: Board -> Int -> Int -> Board
moveVehicle b i n = (iteRate b i n)


main = putStrLn $ show $ initialBoard [(H, (2,0), 2), (V, (0,2), 3)]
