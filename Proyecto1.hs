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
  
psm ::Board -> Int -> Int -> Int
psm b k n = if isValidMove b k (n-1) then psm b k (n-1) else n

vismuto ::Board->Int->Int->(Int,Int)
vismuto xs k l
  | k >= l = (0,0)
  | (psm xs k 0) == 0 = vismuto xs (k+1) l
  | otherwise = (k, psm xs k 0)
-- Genera todos los tableros válidos a partir del estado actual
getNeighbors :: Board -> [Board]
getNeighbors currentBoard = 
    [ moveVehicle currentBoard vehicleIdx steps | 
      vehicleIdx <- [0 .. (length currentBoard - 1)], -- Para cada vehículo
      steps <- [-4 .. 4],                             -- Probamos desplazamientos (ajusta el rango si quieres)
      steps /= 0,                                     -- No nos interesa mover 0 pasos
      isValidMove currentBoard vehicleIdx steps       -- Solo si el movimiento es válido
    ]

solveRushHour :: Board -> (Int, [Board])
solveRushHour initial = bfs [(initial, [])] [] -- Cola inicial, Lista de visitados vacía

-- Función auxiliar bfs
-- bfs :: Cola -> Visitados -> Resultado
bfs :: [(Board, [Board])] -> [Board] -> (Int, [Board])
bfs [] _ = (0, []) -- No hay solución (o manejar error)
bfs ((current, path):queue) visited
    -- 1. Caso Base: ¿Ganamos?
    | isSolved current = (length path, reverse (current:path)) -- Retorna (cantidad, camino ordenado)
    
    -- 2. Optimización: ¿Ya visitamos este estado?
    | current `elem` visited = bfs queue visited 
    
    -- 3. Paso Recursivo: Expandir vecinos
    | otherwise = bfs (queue ++ nextStates) (current:visited)
  where
    -- Generamos los vecinos que NO hemos visitado aún
    nextStates = [ (nextB, current:path) | 
                   nextB <- getNeighbors current, 
                   not (nextB `elem` visited) ]

isValidMove :: Board -> Int -> Int -> Bool
isValidMove b k n
    | n == 0    = True -- No moverse siempre es "válido" (aunque inútil)
    | otherwise = all isSafe steps
  where
    original = b !! k
    -- Obtenemos los demás vehículos quitando el que se va a mover (usando take y drop por índice es más seguro que filter)
    others = take k b ++ drop (k+1) b
    
    -- Generamos la lista de pasos intermedios.
    -- Si n=2, steps=[1,2]. Si n=-2, steps=[-1,-2].
    steps = if n > 0 then [1..n] else [-1, -2 .. n]
    
    -- Función auxiliar que mueve el original 's' pasos y verifica si choca
    isSafe s = verify (move original s) others

isSolved::Board->Bool
isSolved ((o,(i,j),l):_) = j >= (6-l)
    
main = putStrLn $ show $  solveRushHour [(H, (2,0), 2), (V, (0,3), 3), (V,(0,4),3)]

