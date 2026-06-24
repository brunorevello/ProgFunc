{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- EVALUACIÓN DE EXPRESIONES -}
{- HLINT ignore "Use camelCase" -}
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
                          Just v  -> Right v
                          Nothing -> Right (ValInt 0)

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

evalExp p s (Call idF exp) = do
  let f = filterFunc p idF
  val <- evalExp p s exp
  case f of 
    (Fun _ idv stmts expF) -> do 
      let mid_s = new idv val (newFrame s)
      final_s <- executeStmts p mid_s stmts
      evalExp p final_s expF


executeStmts :: Prog -> State -> [Stmt] -> Either RuntimeError State
executeStmts _ s [] = Right s
executeStmts p s ((Assign idv exp):ss) = do 
  val <- evalExp p s exp
  let s_assign = set idv val s
  executeStmts p s_assign ss
executeStmts p s ((While exp stmts):ss) = do 
  b <- evalExp p s exp
  case b of 
    ValBool True -> do 
      while_s <- executeStmts p s stmts
      executeStmts p while_s (While exp stmts:ss)
    ValBool False -> executeStmts p s ss
executeStmts p s ((If exp stmts1 stmts2):ss) = do 
  b <- evalExp p s exp
  case b of 
    ValBool True -> executeStmts p s (stmts1 ++ ss) -- hola
    ValBool False -> executeStmts p s (stmts2 ++ ss)
executeStmts p s ((Case exp clauses):ss) = do
  val <- evalExp p s exp
  let (stmts, s_case) = match_clause val clauses s
  executeStmts p s_case (stmts ++ ss)

filterFunc :: Prog -> Id -> Fun
filterFunc (Fun id idv stmts exp:fs) idF
  | id == idF = Fun id idv stmts exp
  | otherwise = filterFunc fs idF


match_clause :: Val -> [Clause] -> State -> (Stmts, State)
match_clause _ [] s = ([], s)
match_clause val ((Clause pat stmts):cs) s =
  let (matched, s') = match_pattern val pat s
  in if matched then (stmts, s') else match_clause val cs s

match_pattern :: Val -> Pattern -> State -> (Bool, State)
match_pattern (ValList [])     PNil          s = (True, s)
match_pattern (ValList (x:xs)) (PCons p1 p2) s =
  let (m1, s1) = match_pattern (ValInt x)   p1 s
      (m2, s2) = match_pattern (ValList xs)  p2 s1
  in (m1 && m2, s2)
match_pattern (ValInt x)  (PLitN n) s = (x == n, s)
match_pattern (ValBool b) (PLitB c) s = (b == c, s)
match_pattern val         (PVar name) s = (True, set name val s)
match_pattern _ _ s = (False, s)