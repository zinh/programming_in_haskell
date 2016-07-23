import Data.Monoid

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

intInts :: Monoid m => (Integer -> m) -> m
intInts mk_m = go [1..100]
  where go [] = mempty
        go (x:xs)
          | let div_by_5 = x `mod` 5 == 0
                div_by_7 = x `mod` 7 == 0
          , (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7))
          = mappend (mk_m x) (go xs)
          | otherwise = go xs

fizzbuzz :: Monoid m => (String -> m) -> m
fizzbuzz f = go [1..100]
  where go [] = mempty
        go (x:xs)
          | (x `mod` 3 == 0) && (x `mod` 5 == 0) = mappend (f "FizzBuzz") (go xs)
          | (x `mod` 3 == 0) = mappend (f "Fizz") (go xs)
          | (x `mod` 5 == 0) = mappend (f "Buzz") (go xs)
          | otherwise = mappend (f (show x)) (go xs)

-- main = do
--   sequence (map print $ fizzbuzz (:[]))
