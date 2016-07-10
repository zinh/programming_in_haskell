module Geometry.Sphere( volumn, area ) where

volumn :: Float -> Float
volumn radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
