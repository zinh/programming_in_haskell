{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actualCodes guessCodes = foldl match 0 $ zip actualCodes guessCodes
  where
    match count (a, b) = 
      if a == b then
        count + 1
      else
        count

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors codes = map (countColor codes) colors
  where
    countColor [] _ = 0
    countColor (x:xs) color =
      if (x == color) then
        1 + (countColor xs color)
      else
        countColor xs color

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actualCodes guessCodes = sum $ map tupleMin $ zip (countColors actualCodes) (countColors guessCodes)
  where 
    tupleMin (a,b) = if a < b then a else b

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact (totalMatch - exact)
  where 
    exact = exactMatches secret guess
    totalMatch = matches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guessCode exactMatch nonexactMatch) codes =
  let (Move _ exactMatch' nonexactMatch') = getMove guessCode codes
  in
    (exactMatch == exactMatch') && (nonexactMatch' == nonexactMatch)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes givenMove = filter (isConsistent givenMove)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
