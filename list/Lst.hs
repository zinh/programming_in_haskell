data Lst a = Empty | Cons a (Lst a) deriving (Show)

lsum :: (Num a) => Lst a -> a
lsum Empty = 0
lsum (Cons a tail) = a + lsum tail

lproduct :: (Num a) => Lst a -> a
lproduct Empty = 1
lproduct (Cons a tail) = a * (lproduct tail)

lhead :: Lst a -> Lst a
lhead Empty = Empty
lhead (Cons hd tl) = tl

ltail :: Lst a -> Maybe (Lst a)
ltail Empty = Nothing
ltail (Cons a tl) = Just tl

setHead :: Lst a -> a -> Lst a
setHead Empty a = Empty
setHead (Cons a tl) b = Cons b tl

ldrop :: Lst a -> Integer -> Lst a
ldrop Empty _ = Empty
ldrop lst 0 = lst
ldrop (Cons hd tl) n = ldrop tl (n - 1)

ldropWhile :: (a -> Bool) -> Lst a -> Lst a
ldropWhile _ Empty = Empty
ldropWhile f (Cons hd tl)
  | f hd = ldropWhile f tl
  | otherwise = tl
