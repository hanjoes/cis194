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

-- This one uses a function to carry states, magic!
extend :: State -> String -> Int -> State
extend s name n = (\x -> if x == name then n else s x)

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

b2i :: Bool -> Int
b2i b = if b then 1 else 0

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE _ (Val n) = n
evalE st (Op lhs bop rhs) = case bop of
  Plus -> evalE st lhs + evalE st rhs
  Minus -> evalE st lhs - evalE st rhs
  Times -> evalE st lhs * evalE st rhs
  Divide -> evalE st lhs `div` evalE st rhs
  Gt -> b2i $ evalE st lhs > evalE st rhs
  Ge -> b2i $ evalE st lhs >= evalE st rhs
  Lt -> b2i $ evalE st lhs < evalE st rhs
  Le -> b2i $ evalE st lhs <= evalE st rhs
  Eql -> b2i $ evalE st lhs == evalE st rhs

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name e) = DAssign name e
desugar (Incr name) = DAssign name (Op (Var name) Plus (Val 1))
desugar (If e st st') = DIf e (desugar st) (desugar st')
desugar (While e st) = DWhile e $ desugar st
desugar (For st e st' st'') = DSequence (desugar st) (DWhile e (DSequence (desugar st'') (desugar st')))
desugar (Sequence st st') = DSequence (desugar st') (desugar st')
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

i2b :: Int -> Bool
i2b n = if n == 1 then True else False

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign name e) = extend s name $ evalE s e
evalSimple s (DIf e st st') = if i2b $ evalE s e then evalSimple s st else evalSimple s st'
evalSimple s (DWhile e st) = if i2b $ evalE s e then evalSimple s (DSequence st (DWhile e st)) else evalSimple s DSkip
evalSimple s (DSequence st st') = evalSimple (evalSimple s st) st'
evalSimple s DSkip = s

run :: State -> Statement -> State
run s st = evalSimple s $ desugar st

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
