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
isValidMove b k n = verify movido (filter(\x -> x /= original) b)
  where
    original = iteRate b k
    movido = move original n
    
iteRate :: [Vehicle] -> Int -> Vehicle
iteRate (x:_) 0  = x
iteRate (_:xs) i 
  | i > 0     = iteRate xs (i-1) 
  | otherwise = error "Carro no encontrado"
  
move:: Vehicle -> Int -> Vehicle
move (o, (i, j), l) n
      | o == H    = (o, (i, j + n), l)
      | otherwise = (o, (i + n, j), l)
      
--Pregunta 3
moveVehicle :: Board -> Int -> Int -> Board
moveVehicle (x:xs) 0 n = (move x n) :xs
moveVehicle (x:xs) k n = x:moveVehicle xs (k-1) n

main = putStrLn $ show $ isValidMove [(H, (2,0), 2), (V, (2,3), 3)] 0 1
