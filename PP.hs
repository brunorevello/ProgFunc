{- TAREA DE PROGRAMACIÓN FUNCIONAL 2026 -}
{- PRETTY-PRINTING -}
module PP where

import AST

-- Pretty-printing de un programa
-- El comportamiento de la función se especifica en la letra de la Tarea.
ppProg :: Prog -> String
ppProg [] = ""
ppProg (x:xs) = ppFun x ++ "\n" ++ ppInnerFunc xs

ppInnerFunc :: Prog -> String 
ppInnerFunc [] = ""
ppInnerFunc (x:xs) = "\n" ++ ppFun x ++ "\n" ++ ppInnerFunc xs

ppFun :: Fun -> String
ppFun (Fun idfun xs stmts exp) = "fun " ++ idfun ++ " " ++ xs ++ " {\n" ++ ppStmts stmts 1 ++ "} " ++ ppExp exp ++ ";"


ppStmts :: Stmts -> Int -> String
ppStmts [] _ = ""
ppStmts ((Assign id exp):xs) n = tab n ++ id ++ " := " ++ ppExp exp ++ ";\n" ++ ppStmts xs n
ppStmts ((While exp stmts):xs) n = tab n ++ "while " ++ ppExp exp ++ " {\n" ++ ppStmts stmts (n + 1) ++ tab n ++ "};\n" ++ ppStmts xs n
ppStmts ((If exp stmts1 stmts2):xs) n = tab n ++ "if " ++ ppExp exp ++ " then {\n" ++ ppStmts stmts1 (n + 1) ++ tab n ++ "} else {\n" ++ ppStmts stmts2 (n + 1) ++ tab n ++ "};\n" ++ ppStmts xs n
ppStmts ((Case exp clauses):xs) n = tab n ++ "case " ++ ppExp exp ++ " of {\n" ++ ppClauses clauses (n + 1) ++ tab n ++ "};\n" ++ ppStmts xs n


ppClauses :: [Clause] -> Int -> String
ppClauses [] _ = ""
ppClauses ((Clause pattern stmts):xs) n = tab n ++ ppPattern pattern ++ " -> {\n" ++ ppStmts stmts (n + 1) ++ tab n ++ "};\n" ++ ppClauses xs n


ppPattern :: Pattern -> String
ppPattern PNil = "Nil"
ppPattern (PCons patt1 patt2) = "(Cons " ++ ppPattern patt1 ++ " " ++ ppPattern patt2 ++ ")"
ppPattern (PLitN i) = show i 
ppPattern (PLitB b) = show b
ppPattern (PVar var) = var


ppExp :: Exp -> String
ppExp (LitN i) = show i
ppExp (LitB b) = show b 
ppExp (Cons exp1 exp2) = "(Cons " ++ ppExp exp1 ++ " " ++ ppExp exp2 ++ ")"
ppExp Nil = "Nil"
ppExp (Head exp) = "(head " ++ ppExp exp ++ ")"
ppExp (Tail exp) = "(tail " ++ ppExp exp ++ ")"
ppExp (Call i exp) = "(" ++ i ++ " " ++ ppExp exp ++ ")"
ppExp (Var i) = i
ppExp (BinOp Add exp1 exp2) = "(" ++ ppExp exp1 ++ " + " ++ ppExp exp2 ++ ")"
ppExp (BinOp Sub exp1 exp2) = "(" ++ ppExp exp1 ++ " - " ++ ppExp exp2 ++ ")"
ppExp (BinOp Times exp1 exp2) = "(" ++ ppExp exp1 ++ " * " ++ ppExp exp2 ++ ")"
ppExp (BinOp Div exp1 exp2) = "(" ++ ppExp exp1 ++ " / " ++ ppExp exp2 ++ ")"
ppExp (BinOp Mod exp1 exp2) = "(" ++ ppExp exp1 ++ " % " ++ ppExp exp2 ++ ")"
ppExp (BinOp And exp1 exp2) = "(" ++ ppExp exp1 ++ " && " ++ ppExp exp2 ++ ")"
ppExp (BinOp Or exp1 exp2) = "(" ++ ppExp exp1 ++ " || " ++ ppExp exp2 ++ ")"
ppExp (BinOp Equ exp1 exp2) = "(" ++ ppExp exp1 ++ " == " ++ ppExp exp2 ++ ")"
ppExp (BinOp Lt exp1 exp2) = "(" ++ ppExp exp1 ++ " < " ++ ppExp exp2 ++ ")"
ppExp (UnOp Minus exp) = "(-" ++ ppExp exp ++ ")"
ppExp (UnOp Not exp) = "(!" ++ ppExp exp ++ ")"


tab :: Int -> String
tab n = concat (replicate n "    ")
