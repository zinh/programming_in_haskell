{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

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
          | item == 1 && p == 1 = "x"
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
-- times = undefined
times (P lst1) (P lst2) = sum $ map (eachProduct lst2) (listWithIndex lst1)
  where eachProduct lst (power, n) = times' n power lst

listWithIndex :: [a] -> [(Int, a)]
listWithIndex = zip [0..]

times' :: Num a => a -> Int -> [a] -> Poly a
times' n power1 lst = P $ shift (resultPower - power2) 0 (map (n*) lst)
  where power2 = (length lst + 1)
        resultPower = power1 + power2

shift :: Num a => Int -> a -> [a] -> [a]
shift 0 _ lst = lst
shift n a lst = shift (n - 1) a (a:lst)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      (P lst) = P $ map ((-1)*) lst
    fromInteger = P . (:[]) . fromInteger
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P lst) n = sum $ map (\(power,param) -> param * (n ^ power)) (listWithIndex lst)

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a
    nderiv n a = nderiv (n - 1) (deriv a)
    deriv a = nderiv 1 a

polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (P lst) = sum $ map derivSingle (tail $ listWithIndex lst)

derivSingle :: Num a => (Int, a) -> Poly a
derivSingle (power, param) = (P [(fromInteger $ toInteger power) * param]) * (x^(power - 1))

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = polyDeriv
