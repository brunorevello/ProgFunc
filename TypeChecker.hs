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

primero :: (a, b, c) -> a
primero (x, _, _) = x

segundo :: (a, b, c) -> b
segundo (_, y, _) = y

trd :: (a, b, c) -> c
trd (_, _, z) = z

checkProg :: Prog -> CheckRes
checkProg p = if null names 
                then if null errTypes then Ok else HasTypeErrors errTypes
                else HasNameErrors names
  where names = checkOverallDupVar p
        errTypes = checkTypes p []

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
checkExp p exp
  | not (null nameErrors) = HasNameErrors nameErrors
  | not (null typeErrors) = HasTypeErrors typeErrors
  | otherwise             = Ok
  where
    funs       = [idF | Fun idF _ _ _ <- p]
    nameErrors = checkExpresion exp funs []
    (typeErrors, _) = checkTExp exp [] []

checkTypes :: Prog -> [TypeError] -> [TypeError]
checkTypes [] errores = errores
checkTypes (Fun idFunc idVar stmts exp : fs) errores = 
    let 
        entornoInicial = [(idVar, TList)] 
        
        -- En vez de checkTStmts, usamos foldl con checkTStmt para obtener errores y el entorno final
        (erroresFinalesStmts, entornoFinal) = foldl (\(errs, env) stmt -> checkTStmt stmt env errs) (errores, entornoInicial) stmts
        
        -- Ahora sí, la expresión de retorno conoce todas las variables inferidas
        (erroresExp, tipoRetorno) = checkTExp exp entornoFinal erroresFinalesStmts
        
        erroresFinalesFun = if tipoRetorno == TList
                            then erroresExp
                            else erroresExp ++ [WrongReturnType idFunc tipoRetorno]
                            
    in checkTypes fs erroresFinalesFun

checkTStmts :: [Stmt] -> [(Id , Type)] -> [TypeError] -> [TypeError]
checkTStmts [] _ errores = errores
checkTStmts (stmt:stmts) ids_con_tipos errores = checkTStmts stmts entornoNuevo erroresActualizados
  where 
    (erroresActualizados, entornoNuevo) = checkTStmt stmt ids_con_tipos errores

checkTStmt :: Stmt -> [(Id , Type)] -> [TypeError] -> ([TypeError], [(Id , Type)])

checkTStmt (Assign id exp) ids_con_tipos errores | not existe = (erroresExp, (id, tipoExp) : ids_con_tipos)
                                                 | tipoExp /= tipoOld = (erroresExp ++ [AssignTypeMismatch id tipoOld tipoExp], ids_con_tipos)
                                                 | otherwise = (erroresExp, ids_con_tipos)
                                              where
                                                resExp     = checkTExp exp ids_con_tipos errores
                                                erroresExp = fst resExp
                                                tipoExp    = snd resExp
                                                func       = existeVariable id ids_con_tipos
                                                existe     = fst func
                                                tipoOld    = snd (snd func)

checkTStmt (While exp stmts) ids_con_tipos errores = (erroresFinales, ids_con_tipos)
  where
    resExp = checkTExp exp ids_con_tipos errores
    erroresExp = fst resExp
    tipoExp = snd resExp
    erroresCond = if tipoExp == TBool 
                  then erroresExp 
                  else erroresExp ++ [CondNotBool tipoExp]              
    erroresFinales = checkTStmts stmts ids_con_tipos erroresCond


checkTStmt (If exp stmts1 stmts2) ids_con_tipos errores = (erroresFinales, ids_con_tipos)
  where
    resExp = checkTExp exp ids_con_tipos errores
    erroresExp = fst resExp
    tipoExp = snd resExp
    erroresCond = if tipoExp == TBool 
                  then erroresExp 
                  else erroresExp ++ [CondNotBool tipoExp]
                  
    erroresThen = checkTStmts stmts1 ids_con_tipos erroresCond
    erroresFinales = checkTStmts stmts2 ids_con_tipos erroresThen

checkTStmt (Case exp clauses) ids_con_tipos errores = (erroresFinales, ids_con_tipos)
  where
    resExp = checkTExp exp ids_con_tipos errores
    erroresExp = fst resExp
    tipoEsperado = snd resExp
    erroresFinales = checkTClauses clauses tipoEsperado ids_con_tipos erroresExp


checkTClauses :: [Clause] -> Type -> [(Id , Type)] -> [TypeError] -> [TypeError]
checkTClauses [] _ _ errores = errores
checkTClauses (Clause pattern stmts : clauses) tipo ids_con_tipos errores = checkTClauses clauses tipo ids_con_tipos erroresActualizados
  where 
    func         = checkTPattern pattern tipo ids_con_tipos []
    erroresPat   = primero func
    entornoPat   = trd func
    erroresStmts = checkTStmts stmts entornoPat erroresPat
    erroresActualizados = errores ++ erroresStmts


checkTPattern :: Pattern -> Type -> [(Id , Type)] -> [TypeError] -> ([TypeError], Type, [(Id, Type)])
checkTPattern PNil tipo ids_con_tipos errores | tipo == TList = (errores, TList, ids_con_tipos)
                                  | otherwise = (PatMismatch tipo TList:errores, TList, ids_con_tipos)

checkTPattern (PCons pattern1 pattern2) tipo ids_con_tipos errores 
  | tipo /= TList             = (PatMismatch tipo TList : errores, TList, ids_con_tipos)
  | t1 == TInt && t2 == TList = (errores ++ errs1 ++ errs2, TList, env1 ++ env2)
  | otherwise                 = (errores ++ [ConsExpType t1 t2], TList, env1 ++ env2)
  where 
    (errs1, t1, env1) = checkTPattern pattern1 TInt ids_con_tipos []
    (errs2, t2, env2) = checkTPattern pattern2 TList ids_con_tipos []
 

checkTPattern (PLitN _) tipo ids_con_tipos errores | tipo == TInt = (errores, TInt, ids_con_tipos)
                                       | otherwise = (PatMismatch tipo TInt:errores, TInt, ids_con_tipos)
checkTPattern (PLitB _) tipo ids_con_tipos errores | tipo == TBool = (errores, TBool, ids_con_tipos)
                                       | otherwise = (PatMismatch tipo TBool:errores, TBool, ids_con_tipos)                                       
checkTPattern (PVar id) tipo ids_con_tipos errores = (errores, tipo, (id, tipo):ids_con_tipos)

checkTExp :: Exp -> [(Id, Type)] -> [TypeError] -> ([TypeError], Type)
checkTExp (LitN _) _ errores = (errores, TInt)
checkTExp (LitB _) _ errores = (errores, TBool)
checkTExp (Cons exp1 exp2) ids_con_tipos errores = (errs2 ++ errT1 ++ errT2, TList)
  where
    (errs1, t1) = checkTExp exp1 ids_con_tipos errores
    (errs2, t2) = checkTExp exp2 ids_con_tipos errs1
    errT1 = if t1 /= TInt  then [ConsExpType t1 TList] else []
    errT2 = if t2 /= TList then [ConsExpType TInt t2]  else []

checkTExp Nil _ errores = (errores, TList)
checkTExp (Head exp) ids_con_tipos errores | snd func == TList = (fst func, TInt)
                                           | otherwise = (fst func ++ [HeadTailArg (snd func)], TInt)
                                          where
                                            func = checkTExp exp ids_con_tipos errores

checkTExp (Tail exp) ids_con_tipos errores | snd func == TList = (fst func, TList)
                                           | otherwise = (fst func ++ [HeadTailArg (snd func)], TList)
                                          where
                                            func = checkTExp exp ids_con_tipos errores

checkTExp (Call id exp) ids_con_tipos errores | snd func == TList = (fst func, TList)
                                              | otherwise = (fst func ++ [CallArgType id (snd func)], TList)
                                              where
                                                func = checkTExp exp ids_con_tipos errores  

checkTExp (Var id) ids_con_tipos errores = (errores, snd (snd func))
                              where
                                func = existeVariable id ids_con_tipos



checkTExp (BinOp Add exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TInt)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType Add (snd func1) (snd func2)], TInt)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Sub exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TInt)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType Sub (snd func1) (snd func2)], TInt)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Times exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TInt)
                                                        | otherwise = (fst func2 ++ [BinOpWrongType Times (snd func1) (snd func2)], TInt)
                                                        where
                                                          func1 = checkTExp exp1 ids_con_tipos errores
                                                          func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Div exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TInt)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType Div (snd func1) (snd func2)], TInt)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Mod exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TInt)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType Mod (snd func1) (snd func2)], TInt)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp And exp1 exp2) ids_con_tipos errores | snd func1 == TBool && snd func2 == TBool = (fst func2, TBool)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType And (snd func1) (snd func2)], TBool)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Or exp1 exp2) ids_con_tipos errores | snd func1 == TBool && snd func2 == TBool = (fst func2, TBool)
                                                     | otherwise = (fst func2 ++ [BinOpWrongType Or (snd func1) (snd func2)], TBool)
                                                     where
                                                       func1 = checkTExp exp1 ids_con_tipos errores
                                                       func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Equ exp1 exp2) ids_con_tipos errores | snd func1 == snd func2 = (fst func2, TBool)
                                                      | otherwise = (fst func2 ++ [BinOpWrongType Equ (snd func1) (snd func2)], TBool)
                                                      where
                                                        func1 = checkTExp exp1 ids_con_tipos errores
                                                        func2 = checkTExp exp2 ids_con_tipos (fst func1)

checkTExp (BinOp Lt exp1 exp2) ids_con_tipos errores | snd func1 == TInt && snd func2 == TInt = (fst func2, TBool)
                                                     | otherwise = (fst func2 ++ [BinOpWrongType Lt (snd func1) (snd func2)], TBool)
                                                     where
                                                       func1 = checkTExp exp1 ids_con_tipos errores
                                                       func2 = checkTExp exp2 ids_con_tipos (fst func1)
                                                        
checkTExp (UnOp Minus exp) ids_con_tipos errores | snd func == TInt = (fst func, TInt)
                                                | otherwise = (fst func ++ [UnOpWrongType Minus (snd func)], TInt)
                                                where
                                                  func = checkTExp exp ids_con_tipos errores 

checkTExp (UnOp Not exp) ids_con_tipos errores | snd func == TBool = (fst func, TBool)
                                                | otherwise = (fst func ++ [UnOpWrongType Not (snd func)], TBool)
                                                where
                                                  func = checkTExp exp ids_con_tipos errores                                                  
