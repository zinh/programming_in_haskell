-- Function basic
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky seven"
lucky x = "Sorry"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Unknown!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVector :: (Num a) => (a,a) -> (a, a) -> (a, a)
addVector (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

first :: (a,b,c) -> a
first (a, _, _) = a

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:tl) = 1 + length' tl

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (hd:tl) = hd + sum'(tl)

-- Using Guard
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b = GT
  | a < b = LT
  | otherwise = EQ

-- Using Guard with Where
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "Underweight"
  | bmi <= normal  = "Normal"
  | bmi <= fat = "Fat"
  | otherwise = "Superman"
  where bmi = weight / height ^ 2
        (skinny,normal,fat) = (18.5, 25.0, 30.0)

-- Using let keyword
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + topArea * 2

-- Let is also an expression
-- 4 * (let a = 10 in a  + 1)
-- [ let square x = x * x in [square 5, square 6, square 7]
-- (let a = 10; b = 11, c = 12 in a + b + c)

-- Using case
head' :: [a] -> a
head' lst = case lst of [] -> error "Empty list"
                        (x:_) 0 -> x
