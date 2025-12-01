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
verify v xs
  | not (null [(i,j) | (i,j)<-positions v, (i<0 || i>5) || (j<0 || j>5)]) = False
  | null xs = True
  | null [y | y<-positions v, elem y (positions (head xs))] = verify v (tail xs)
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
    
move:: Vehicle -> Int -> Vehicle
move (o, (i, j), l) n
      | o == H    = (o, (i, j + n), l)
      | otherwise = (o, (i + n, j), l)

iteRate :: [Vehicle] -> Int -> Vehicle
iteRate [] _ = (H,(0,0),0)
iteRate (x:_) 0  = x
iteRate (_:xs) i 
  | i > 0     = iteRate xs (i-1)
  | otherwise = error "Vehiculo no encontrado"

--Pregunta 3
moveVehicle :: Board -> Int -> Int -> Board
moveVehicle (x:xs) 0 n = (move x n) :xs
moveVehicle (x:xs) k n = x:moveVehicle xs (k-1) n

--Pregunta 4
vllc:: Board->Int->Int->Int
vllc xs k n
  | (isValidMove xs k (n+1)) = vllc xs k (n+1)
  | otherwise = n

main = putStrLn $ show $ vllc [(H,(0,1),5)] 0 0
