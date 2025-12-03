-- Amanda Mendoza 31708119 y Carlos Heydra 31307754

type Coord = (Int, Int)
data Orientation = H | V deriving(Show, Eq)
type Vehicle = (Orientation, Coord, Int)
type Board = [Vehicle]

-- Pregunta 1
initialBoard :: [Vehicle] -> Board
initialBoard [] = []
initialBoard [x] = [x]
initialBoard (x:xs)
  | verify x xs = if null (initialBoard xs) then [] else x:xs
  | otherwise = []

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

--Pregunta 2
isValidMove :: Board -> Int -> Int -> Bool
isValidMove b k n
    | n == 0    = True
    | otherwise = all isSafe steps
  where
    original = iteRate b k
    
    others = filter (\x->x /= original) b
    
    steps = if n > 0 then [1..n] else [-1, -2 .. n]
    
    isSafe s = verify (move original s) others
-- Se revisa si a lo largo de todo el trayecto hay algún choque
    
move:: Vehicle -> Int -> Vehicle
move (o, (i, j), l) n
      | o == H    = (o, (i, j + n), l)
      | otherwise = (o, (i + n, j), l)

iteRate :: Board -> Int -> Vehicle
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
solveRushHour :: Board -> (Int, [Board])
solveRushHour b = bfs [(b, [])] []

bfs :: [(Board, [Board])] -> [Board] -> (Int, [Board])
bfs [] _ = (0, [])
bfs ((current, path):queue) visited
    | isSolved current = (length path, reverse (current:path))
    | elem current visited = bfs queue visited 
    | otherwise = bfs (queue ++ nextStates) (current:visited)
  where
    nextStates = [(nextBoard, current:path) | nextBoard <- getNeighbors current, not (elem nextBoard visited)]

getNeighbors :: Board -> [Board]
getNeighbors b = 
    [moveVehicle b k n |
      k <- [0 .. (length b - 1)],
      n <- [-5 .. 5],
      n /= 0,
      isValidMove b k n
    ]

isSolved::Board->Bool
isSolved ((_,(_,j),l):_) = j >= (6-l)
