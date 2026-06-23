{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- EVALUACIÓN DE EXPRESIONES -}
module Eval where

import AST
import State
import Data.Char (digitToInt)


-- Resultado de una evaluación
type EvalRes = Either RuntimeError Val

-- Errores en tiempo de ejecución
data RuntimeError
  = HeadOfEmptyList
  | TailOfEmptyList
  | DivisionByZero
  deriving Eq

instance Show RuntimeError where
  show HeadOfEmptyList = "head of empty list"
  show TailOfEmptyList = "tail of empty list"
  show DivisionByZero  = "division by zero"

  
-- Evalúa una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
evalExp :: Prog -> State -> Exp -> EvalRes
evalExp _ _ (LitN int)  = Right (ValInt int)
evalExp _ _ (LitB bool) = Right (ValBool bool)
evalExp _ _ Nil         = Right (ValList [])

evalExp p s (Head exp)  = do  
                        xs <- evalExp p s exp
                        case xs of 
                          ValList []    -> Left HeadOfEmptyList
                          ValList (x:_) -> Right (ValInt x)
evalExp p s (Tail exp)  = do
                        xs <- evalExp p s exp
                        case xs of 
                          ValList []     -> Left TailOfEmptyList
                          ValList (_:xs) -> Right (ValList xs)

evalExp _ s (Var id)    = case get id s of 
                          Nothing -> Right (ValInt 0)
                          Just v  -> Right v

evalExp p s (Cons exp1 exp2)                = do
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of
    (ValInt x, ValList xs) -> Right (ValList (x:xs))
evalExp p s (BinOp Add exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValInt a, ValInt b) -> Right (ValInt (a+b))
evalExp p s (BinOp Sub exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValInt a, ValInt b) -> Right (ValInt (a-b))
evalExp p s (BinOp Times exp1 exp2)          = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValInt a, ValInt b) -> Right (ValInt (a*b))
evalExp p s (BinOp Mod exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValInt a, ValInt b) -> Right (ValInt (mod a b))
evalExp p s (BinOp Div exp1 exp2)           = do
  a <- evalExp p s exp1
  b <- evalExp p s exp2 
  case b of 
    ValInt 0 -> Left DivisionByZero
    ValInt y -> case a of 
      ValInt x -> Right (ValInt (div x y))
evalExp p s (UnOp Minus exp)                = do 
  a <- evalExp p s exp
  case a of 
    ValInt x -> Right (ValInt (-x))

evalExp p s (UnOp Not exp)                  = do
  a <- evalExp p s exp
  case a of 
    ValBool x -> Right (ValBool (not x))


evalExp p s (BinOp And exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValBool x, ValBool y) -> Right (ValBool (x && y))
evalExp p s (BinOp Or exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValBool x, ValBool y) -> Right (ValBool (x || y))
evalExp p s (BinOp Equ exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of 
    (ValBool x, ValBool y) -> Right (ValBool (x == y))
    (ValInt x, ValInt y) -> Right (ValBool (x == y))
    (ValList x, ValList y) -> Right (ValBool (x == y))
evalExp p s (BinOp Lt exp1 exp2)           = do 
  a <- evalExp p s exp1
  b <- evalExp p s exp2
  case (a, b) of
    (ValInt x, ValInt y)   -> Right (ValBool (x < y))
    (ValBool x, ValBool y) -> Right (ValBool (x < y))

evalExp p s (Call id exp) = Right (ValInt 0)
