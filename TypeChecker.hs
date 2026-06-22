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
checkProg p = if null names then Ok else HasNameErrors names
  where names = checkOverallDupVar p


-- Collect ids of duplicated functions
checkOverallDupFunc :: Prog -> [NameError]
checkOverallDupFunc p = snd (foldl checkDupFunc ([], []) p)

checkDupFunc :: ([Id], [NameError]) -> Fun -> ([Id], [NameError])
checkDupFunc (funs, errors) (Fun idF _ _ _)
  | elem idF funs = (funs, DupFun idF:errors)
  | otherwise     = (idF:funs, errors)


-- Collect other errors from inner functions
checkOverallDupVar :: Prog -> [NameError]
checkOverallDupVar p = snd (foldl checkDupVar ([],[]) p)

checkDupVar :: ([Id], [NameError]) -> Fun -> ([Id], [NameError])
checkDupVar (funs, errors) (Fun idF idV stmts exp) = (funsVars , errors ++ errorsFuns ++ errorsVars ++ checkExpresion exp funsVars varsVars)
  where (funsVars, varsVars, errorsVars) = checkStmts (idF:funs, [idV], []) stmts
        (_, errorsFuns) = checkDupFunc (funs, []) (Fun idF idV stmts exp)



checkStmts :: ([Id], [Id], [NameError]) -> Stmts -> ([Id], [Id], [NameError])
checkStmts = foldl checkStmt

checkStmt :: ([Id], [Id], [NameError]) -> Stmt -> ([Id], [Id], [NameError])
checkStmt (funs, vars, errors) (Assign id exp) = (funs, id:vars, errors ++ checkExpresion exp funs vars)
checkStmt (funs, vars, errors) (While exp stmts) = (funs, vars, errors ++ checkExpresion exp funs vars ++ thrd (checkStmts (funs, vars, []) stmts))
checkStmt (funs, vars, errors) (If exp stmts1 stmts2) = (funs, vars, errors ++ checkExpresion exp funs vars ++ thrd (checkStmts (funs, vars, []) stmts1) ++ thrd (checkStmts (funs, vars, []) stmts2))
checkStmt (funs, vars, errors) (Case exp clauses) = (funs, vars, errors ++ checkExpresion exp funs vars ++ checkClauses (funs, vars, []) clauses)


patDupVars :: [Id] -> [Id] -> [NameError]
patDupVars xs ys = [DupVar x | x <- xs, elem x ys]

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

checkClauses :: ([Id], [Id], [NameError]) -> [Clause] -> [NameError]
checkClauses acum clauses= thrd (foldl checkClause acum clauses)


checkClause ::  ([Id], [Id], [NameError]) -> Clause -> ([Id], [Id], [NameError])
checkClause (funs, vars, errors) (Clause pattern stmts)  = (funs, vars, pattErrors ++ errors ++ thrd (checkStmts (funs, vars ++ pattVars, []) stmts))
  where (pattVars, pattErrors) = checkPatterns pattern vars

checkPatterns :: Pattern -> [Id] -> ([Id], [NameError])
checkPatterns PNil _ = ([], [])
checkPatterns (PCons pattern1 pattern2) vars = (patVars1 ++ patVars2, patErrors1 ++ patErrors2 ++ patDupVars patVars1 patVars2)
                                              where
                                                (patVars1, patErrors1) = checkPatterns pattern1 vars
                                                (patVars2, patErrors2) = checkPatterns pattern2 vars
checkPatterns (PLitN _) _ = ([], [])
checkPatterns (PLitB _) _ = ([], [])
checkPatterns (PVar pid) vars
  | elem pid vars = ([pid], [DupVar pid])
  | otherwise     = ([pid], [])

checkExpresion :: Exp -> [Id] -> [Id] -> [NameError]
checkExpresion (LitN int) _ _ = []
checkExpresion (LitB bool) _ _ = []
checkExpresion (Cons exp1 exp2) funs vars = checkExpresion exp1 funs vars ++ checkExpresion exp2 funs vars
checkExpresion Nil _ _ = []
checkExpresion (Head exp) funs vars  = checkExpresion exp funs vars
checkExpresion (Tail exp) funs vars  = checkExpresion exp funs vars
checkExpresion (Call id exp) funs vars
  | elem id funs = checkExpresion exp funs vars
  | otherwise = UndefFun id : checkExpresion exp funs vars
checkExpresion (Var id) funs vars
  | elem id vars = []
  | otherwise = [UndefVar id]
checkExpresion (UnOp _ exp) funs vars = checkExpresion exp funs vars
checkExpresion (BinOp _ exp1 exp2) funs vars= checkExpresion exp1 funs vars ++ checkExpresion exp2 funs vars

-- Chequeo de una expresión.
-- El comportamiento de la función se especifica en la letra de la Tarea.
checkExp :: Prog -> Exp -> CheckRes
checkExp _ _ = Ok


-- checkTypes :: Prog -> [TypeError]-> [TypeError]
-- checkTypes [] errores = errores
-- checkTypes ((Fun idFunc idVar stmts exp):fs) errores = checkTStmts stmts [] errores ++ checkTExp exp errores ++ checkTypes fs errores

-- checkTStmts :: [Stmt] -> [(Id , Val)] -> [TypeError] -> [TypeError]
-- checkTStmts [] _ errores = errores
-- checkTStmts (stmt:stmts) ids errores = fst func ++ checkStmts stmts (snd func) errores
--                       where func = checkTStmt stmt ids

-- checkTStmt :: Stmt -> [(Id , Val)] -> [TypeError] -> ([TypeError], [(Id , Val)])
-- checkTStmt (Assign id exp) ids_con_tipos errores | not fst func = (errores, (id, getType exp):ids_con_tipos)
--                                                  | fst func && (getType exp <> snd (snd func)) = ((AssignTypeMismatch id (snd (snd func)) getType exp):errores)
--                                                  | otherwise = (errores, ids_con_tipos)
--                                          where 
--                                           func = (existeVariable id ids_con_tipos)
-- checkTStmt (While exp stmts) ids_con_tipos errores = checkTBool exp errores ++ fst (checkTStmts stmts ids_con_tipos errores)
-- checkTStmt (If exp stmts1 stmts2) ids_con_tipos errores = checkBool exp errores ++ fst (checkTStmts stmts1 ids_con_tipos errores) ++ fst (checkTStmts stmts2 ids_con_tipos errores)
-- checkTStmt (Case exp clauses) ids_con_tipos errores = checkTBool exp errores ++ checkTClauses clauses ids_con_tipos errores

-- checkTBool :: Exp -> [TypeError] -> [TypeError]
-- checkTBool exp errores | TypeExp exp == Boolean = []
--                        | otherwise = CondNotBool TypeExp exp
