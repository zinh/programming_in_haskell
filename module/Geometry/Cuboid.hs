module Geometry.Cuboid( volumn, area ) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = 2 * rectangleArea a b + 2 * rectangleArea b c + 2 * rectangleArea c a

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
