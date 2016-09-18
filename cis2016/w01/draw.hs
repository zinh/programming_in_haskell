{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

data TrafficLight = Red | Yellow | Green

greenCircle :: Picture
greenCircle = colored green (solidCircle 1)

redCircle :: Picture
redCircle = colored red (solidCircle 1)

yellowCircle :: Picture
yellowCircle = colored yellow (solidCircle 1)

blankCircle :: Picture
blankCircle = circle 1

topCircle :: Picture -> Picture
topCircle circle = translated 0 2.25 circle

bottomCircle :: Picture -> Picture
bottomCircle circle = translated 0 (-2.25) circle

frame :: Picture
frame = rectangle 2.5 7

trafficLight :: TrafficLight -> Picture
trafficLight Red = (topCircle redCircle) & blankCircle & (bottomCircle blankCircle)
trafficLight Green = (topCircle blankCircle) & blankCircle & (bottomCircle greenCircle)
trafficLight Yellow = (topCircle blankCircle) & yellowCircle & (bottomCircle blankCircle)

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod` 3 == 0 = trafficLight Red
  | round (t/3) `mod` 3 == 1 = trafficLight Green
  | otherwise = trafficLight Yellow

lights :: Int -> Picture
lights 0 = blank
lights n = (trafficLight Red) & translated 3 0 (lights (n - 1))

tree :: Int -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (rotated (pi / 10) (tree (n - 1)) & rotated (-pi / 10) (tree (n - 1)))

treeBloom :: Int -> Double -> Picture
treeBloom 0 _ = blank
treeBloom n f = path [(0,0),(0,1)] & translated 0 1 (rotated (f * pi / 10) (treeBloom (n - 1) f) & rotated (-f * pi / 10) (treeBloom (n - 1) f))

main :: IO()
main = animationOf (treeBloom 8 . sin)
