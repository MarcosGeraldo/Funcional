module Aula1 where
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n =  fibo (n - 1) + fibo (n - 2)


data Point = Point Float Float

data Shape = Rectangle Point Float Float | Circle Point Float | Triangle Point Point Point

dist :: Point -> Point -> Float
dist (Point x y) (Point x1 y1) = (x - x1)^2 * (y - y1)^2 /2

area :: Shape -> Float
area (Rectangle _ x y) = x * y
area (Circle _ x)      = pi * (x*x)
area (Triangle (Point a1 a2) (Point b1 b2) (Point c1 c2))  =  (a1 * a2) + (b1 * b2) + (c1 * c2)
