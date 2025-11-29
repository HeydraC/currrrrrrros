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
verify (o1,(i1,j1),l1) ((o2,(i2,j2),l2) : xs)
  | null [x | x<-positions (o1,(i1,j1),l1), elem x (positions (o2,(i2,j2),l2))] = verify (o1,(i1,j1),l1) xs
  | otherwise = False
