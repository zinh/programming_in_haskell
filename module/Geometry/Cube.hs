module Geometry.Cube( volumn, area ) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volumn side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
