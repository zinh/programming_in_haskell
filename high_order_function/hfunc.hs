-- Curring:
-- let f = mulThree 1 2
-- f 3
mulThree :: (Num a) => a -> a -> a -> a
mulThree a b c = a * b * c

-- Curring with infix
-- let f = (/10)
-- f 10

-- passing function
-- applyTwice (+3) 10
applyTwice :: (a -> a) -> a -> a
applyTwice f a = f a

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ a b
  | null a || null b = []
zipWith' f (hd1:tl1) (hd2:tl2) = f hd1 hd2 : zipWith' f tl1 tl2

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (hd:tl) = f hd : map' f tl

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (hd:tl)
  | f hd = hd : filter' f tl
  | otherwise = filter' f tl

largestDivisible :: (Integral a) => a
largestDivisible = head (filter' p [100000, 99999..])
  where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (hd:tl)
  | f hd = hd : takeWhile' f tl
  | otherwise = []

-- Sum of odd square less than n
sumOdd :: (Integral a) => a -> a
sumOdd n = sum (takeWhile' (< n) (filter odd (map (^2) [1..])))

-- Collatz chain
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain(n `div` 2)
  | otherwise = n:chain(n * 3 + 1)

-- Lambda
-- Syntax: \x y z -> x + y + z

-- Function composition with .
-- map (\x -> negate (abs x)) [-1, -2, 1, 10]
-- map (negate . abs) [-1, -2, 1, 2]

-- Function application with $
-- sum (map sqrt [1..100])
-- sum $ map sqrt [1..100]
oddSum n =
  let oddSquare = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (< n) oddSquare
  in sum belowLimit
