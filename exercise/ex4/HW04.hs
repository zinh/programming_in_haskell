{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  P [] == P [] = True
  P (z:zs) == P (y:ys)
    | z == y = (P zs) == (P ys)
    | otherwise = False
  _ == _ = False
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show p = intercalate " + " (show' p 0 [])

show' :: (Show a, Num a, Eq a) => Poly a -> Int -> [String] -> [String]
show' (P []) _ results = results
show' (P (m:ms)) current_power results 
  | m == 0 = show' (P ms) (current_power + 1) results
  | otherwise = show' (P ms) (current_power + 1) ((formatPower m current_power):results)
  where formatPower item p
          | item == 1 && p == 0 = show item
          | item == 1 = "x^" ++ (show p)
          | p == 0 = show item
          | p == 1 = show item ++ "x"
          | otherwise = (show item) ++ "x^" ++ (show p)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P lst1) (P lst2) = P $ reverse (plusList lst1 lst2 [])
  where
    plusList [] lst results = (reverse lst) ++ results
    plusList lst [] results = (reverse lst) ++ results
    plusList (y:ys) (z:zs) results = plusList ys zs ((y + z):results)

-- Exercise 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P lst1) (P lst2) = foldl (\memo p -> memo + p)(P []) $foldl (\memo n -> (P (map (*n) lst1)) : memo) [] lst2

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

