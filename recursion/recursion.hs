max' :: (Ord a) => [a] -> a
max' [] = error "Empty list"
max' [x] = x
max' (hd:tl) = max hd (max' tl)

-- replicate
repl :: (Num i, Ord i) => i -> a -> [a]
repl 0 _ = []
repl n a = a:(repl (n - 1) a)

-- take
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (hd:tl) = hd : take' (n - 1) tl

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (hd:tl) = reverse' tl ++ [hd]

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (hd:tl) =
  let left = [a | a <- tl, a <= hd]
      right = [a | a <- tl, a > hd]
  in quicksort(left) ++ [hd] ++ quicksort(right)
