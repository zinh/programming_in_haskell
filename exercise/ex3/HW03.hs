module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend currentState k v = \ name ->  if name == k then v else currentState name
empty :: State
empty = \ _ -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var varName) = state varName
evalE state (Val value) = value
evalE state (Op expr1 op expr2) = case op of
  Plus -> (evalE state expr1) + (evalE state expr2)
  Minus -> (evalE state expr1) - (evalE state expr2)
  Times -> (evalE state expr1) * (evalE state expr2)
  Divide -> (evalE state expr1) `div` (evalE state expr2)
  Gt -> if (evalE state expr1) > (evalE state expr2) then 1 else 0
  Ge -> if (evalE state expr1) >= (evalE state expr2) then 1 else 0
  Lt -> if (evalE state expr1) < (evalE state expr2) then 1 else 0
  Le -> if (evalE state expr1) <= (evalE state expr2) then 1 else 0
  Eql -> if (evalE state expr1) == (evalE state expr2) then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Incr varName) = DAssign varName (Op (Var varName)  Plus (Val 1))
desugar (Assign varName expr) = DAssign varName expr
desugar (If expr stm1 stm2) = DIf expr (desugar stm1) (desugar stm2)
desugar (While expr stm) = DWhile expr (desugar stm)
desugar (Sequence stm1 stm2) = DSequence (desugar stm1) (desugar stm2)
desugar Skip = DSkip
desugar (For initStm loopCond updateStm bodyStm) =
  DSequence (desugar initStm) (DWhile loopCond (DSequence (desugar bodyStm) (desugar updateStm)))


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple currentState (DAssign varName expr) = extend currentState varName (evalE currentState expr)
evalSimple currentState (DIf cond trueStm elseStm) =
  let condValue = evalE currentState cond
  in
    if condValue == 0 then (evalSimple currentState elseStm) else (evalSimple currentState trueStm)
evalSimple currentState (DWhile expr bodyStm) =
  let condValue = evalE currentState expr
      newState = evalSimple currentState bodyStm
  in
    if condValue == 0 then currentState else (evalSimple newState (DWhile expr bodyStm))
evalSimple currentState (DSequence stm1 stm2) = evalSimple (evalSimple currentState stm1) stm2
evalSimple currentState DSkip = currentState

run :: State -> Statement -> State
run currentState stm = evalSimple currentState (desugar stm)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
