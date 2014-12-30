module Custom
( Point(..)
, Shape(..)
, surface
, nudge
, nudgePoint
) where

data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle point r) a b = Circle (nudgePoint point a b) r
nudge (Rectangle point1 point2) a b = Rectangle (nudgePoint point1 a b) (nudgePoint point2 a b)

nudgePoint :: Point -> Float -> Float -> Point
nudgePoint (Point x y) a b = Point (x+a) (y+b)
