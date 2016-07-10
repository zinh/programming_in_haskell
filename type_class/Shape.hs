data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Vector a = Vector a a a deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle p1 p2) = (distance p1) * (distance p2)

distance :: Point -> Float
distance (Point x y) = abs $ x - y

translate :: Shape -> Float -> Float -> Shape
translate (Circle p1 r) t_x t_y = Circle (move p1 t_x t_y) r
translate (Rectangle p1 p2) t_x t_y = Rectangle (move p1 t_x t_y) (move p2 t_x t_y)

move :: Point -> Float -> Float -> Point
move (Point x y) t_x t_y = Point (x + t_x) (y + t_y)

scalaProduct :: (Num a) => Vector a -> Vector a -> a
scalaProduct (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector x y z) `vectMult` a = Vector (x * a) (y * a) (z * a)

vectPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vectPlus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
