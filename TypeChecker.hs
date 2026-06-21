{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- CHEQUEO DE NOMBRES Y TIPOS -}
module TypeChecker where

import AST

-- Tipos
data Type = TInt | TBool | TList
  deriving Eq

-- Resultado de un Chequeo
data CheckRes = Ok
              | HasNameErrors [NameError]
              | HasTypeErrors [TypeError]

-- Errores de Nombres
data NameError
  = UndefVar Id
  | UndefFun Id
  | DupFun Id
  | DupVar Id

-- Errores de Tipos
data TypeError
  = CallArgType Id Type
  | BinOpWrongType BOp Type Type
  | UnOpWrongType UOp Type
  | CondNotBool Type
  | AssignTypeMismatch Id Type Type
  | PatMismatch Type Type
  | ConsExpType Type Type
  | HeadTailArg Type
  | WrongReturnType Id Type


-- Instancias de Show de tipos y resultados
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show TList = "list"

instance Show NameError where
  showsPrec _ err = case err of
    UndefVar x ->
      showString "undefined variable: " . showString x

    UndefFun f ->
      showString "undefined function: " . showString f

    DupFun f ->
      showString "duplicated function: " . showString f

    DupVar v ->
      showString "duplicated variable: " . showString v

instance Show TypeError where
  showsPrec _ err = case err of
    CallArgType f t ->
      showString "invalid argument type in "
      . showString f
      . showString ": "
      . shows t

    BinOpWrongType bop t1 t2 ->
      showString "invalid argument type/s in operator "
      . shows bop
      . showString ": "
      . shows t1
      . showString ", "
      . shows t2

    UnOpWrongType uop t ->
      showString "invalid argument type in unary operator "
      . shows uop
      . showString ": "
      . shows t

    CondNotBool t ->
      showString "invalid condition type: "
      . shows t

    AssignTypeMismatch x t1 t2 ->
      showString "invalid assignment in "
      . showString x
      . showString ": expected "
      . shows t1
      . showString ", actual "
      . shows t2

    PatMismatch t1 t2 ->
      showString "invalid pattern: expected "
      . shows t1
      . showString ", actual "
      . shows t2

    ConsExpType t1 t2 ->
      showString "invalid argument type/s in Cons: "
      . shows t1
      . showString ", "
      . shows t2

    HeadTailArg t ->
      showString "invalid list argument type: "
      . shows t

    WrongReturnType f t ->
      showString "invalid return type in "
      . showString f
      . showString ": "
      . shows t

instance Show CheckRes where
  showsPrec _ Ok = showString "ok"
  showsPrec _ (HasNameErrors errs) = showLines errs
  showsPrec _ (HasTypeErrors errs) = showLines errs

showLines :: Show a => [a] -> ShowS
showLines =
  foldr1 (\x acc -> x . showChar '\n' . acc) . map shows


-- Chequeo de un programa.
-- El comportamiento de la función se especifica en la letra de la Tarea.

sonDistintas :: Eq a => [a] -> [a] -> Bool
sonDistintas [] _ = True
sonDistintas (x:xs) ys
    | elem x ys = False
    | otherwise   = sonDistintas xs ys

existeVariable :: Id -> [(Id, Type)] -> (Bool, (Id, Type))
existeVariable i [] = (False, (i, TInt)) 

existeVariable i ((x, t):xs)
    | i == x    = (True, (x, t))
    | otherwise = existeVariable i xs

checkProg :: Prog -> CheckRes
checkProg p = if not (checkNames p []) then HasNameErrors
              else if not (checkTypes p []) then HasTypeErrors --  not checkTypes p then HasTypeErrors
              else Ok

checkNames :: Prog -> [Id] -> Bool
checkNames [] _ = True
checkNames ((Fun idFunc idVar stmts exp):fs) funs
              | elem idFunc funs = False
              | otherwise = checkFunc stmts exp idVar idFunc:funs && checkNames fs idFunc:funs

checkFunc :: Stmts -> Exp -> Id -> [Id] -> Bool
checkFunc stmts exp idVar funs = fst (func) && checkExpresion exp snd (func)
                    where func = checkStmts stmts funs [idVar]

checkStmts :: Stmts -> [Id] -> [Id] -> (Bool, [Id])
checkStmts [] _ vars = (True, vars)
checkStmts (stmt:stmts) funs vars | not (fst func) = (False, [])
                                  | otherwise = checkStmts stmts funs (snd func)
                                  where func = checkStmt stmt funs vars

checkStmt :: Stmt -> [Id] -> [Id] -> (Bool, [Id])
checkStmt (Assign id exp) funs vars = (checkExpresion exp funs vars, id:vars)
checkStmt (While exp stmts) funs vars = (checkExpresion exp funs vars && fst (checkStmts stmts funs vars), vars)
checkStmt (If exp stmts1 stmts2) funs vars = (checkExpresion exp funs vars && fst (checkStmts stmts1 funs vars) && fst (checkStmts stmts2 funs vars), vars)
checkStmt (Case exp clauses) funs vars = (checkExpresion exp funs vars && checkClauses clauses funs vars, vars)

checkClauses :: [Clause] -> [Id] -> [Id] -> Bool
checkClauses [] _ _ = True
checkClauses ((Clause pattern stmts):clauses) funs vars = checkPatterns pattern funs vars && fst (checkStmts stmts funs vars) && checkClauses clauses

checkPatterns :: Pattern -> [Id] -> (Bool, [Id])
checkPatterns PNil _ = (True, [])
checkPatterns (PCons pattern1 pattern2) vars = (fst f1 && fst f2 && (sonDistintas (snd f1) (snd f2)), snd f1 ++ snd f2)
                                                where
                                                  f1 = checkPatterns pattern1 vars
                                                  f2 = checkPatterns pattern2 vars
checkPatterns (PLitN _) _ = (True, [])
checkPatterns (LitB _) _ = (True, [])
checkPatterns (PVar pid) vars = (sonDistintas [pid] vars, [pid]) 


checkExpresion :: Exp -> [Id] -> [Id] -> Bool
checkExpresion _ _ _ = True

-- Chequeo de una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp _ _ = Ok


checkTypes :: Prog -> [TypeError]-> [TypeError]
checkTypes [] errores = errores
checkTypes ((Fun idFunc idVar stmts exp):fs) errores = checkTStmts stmts [] errores ++ checkTExp exp errores ++ checkTypes fs errores

checkTStmts :: [Stmt] -> [(Id , Val)] -> [TypeError] -> [TypeError]
checkTStmts [] _ errores = errores
checkTStmts (stmt:stmts) ids errores = fst func ++ checkStmts stmts (snd func) errores
                      where func = checkTStmt stmt ids

checkTStmt :: Stmt -> [(Id , Val)] -> [TypeError] -> ([TypeError], [(Id , Val)])
checkTStmt (Assign id exp) ids_con_tipos errores | not fst func = (errores, (id, getType exp):ids_con_tipos)
                                                 | fst func && (getType exp <> snd (snd func)) = ((AssignTypeMismatch id (snd (snd func)) getType exp):errores)
                                                 | otherwise = (errores, ids_con_tipos)
                                         where 
                                          func = (existeVariable id ids_con_tipos)
checkTStmt (While exp stmts) ids_con_tipos errores = 