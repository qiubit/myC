-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParMyC where
import AbsMyC
import LexMyC
import ErrM

}

%name pProgram Program
%name pListStmt ListStmt
%name pListExp ListExp
%name pListDec ListDec
%name pStmt Stmt
%name pExp8 Exp8
%name pExp7 Exp7
%name pExp6 Exp6
%name pExp5 Exp5
%name pExp4 Exp4
%name pExp3 Exp3
%name pExp2 Exp2
%name pExp1 Exp1
%name pExp Exp
%name pType Type
%name pDec Dec
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!=' { PT _ (TS _ 1) }
  '&&' { PT _ (TS _ 2) }
  '(' { PT _ (TS _ 3) }
  ')' { PT _ (TS _ 4) }
  '*' { PT _ (TS _ 5) }
  '+' { PT _ (TS _ 6) }
  '++' { PT _ (TS _ 7) }
  ',' { PT _ (TS _ 8) }
  '-' { PT _ (TS _ 9) }
  '--' { PT _ (TS _ 10) }
  '/' { PT _ (TS _ 11) }
  ';' { PT _ (TS _ 12) }
  '<' { PT _ (TS _ 13) }
  '<=' { PT _ (TS _ 14) }
  '=' { PT _ (TS _ 15) }
  '==' { PT _ (TS _ 16) }
  '>' { PT _ (TS _ 17) }
  '>=' { PT _ (TS _ 18) }
  'bool' { PT _ (TS _ 19) }
  'false' { PT _ (TS _ 20) }
  'if' { PT _ (TS _ 21) }
  'int' { PT _ (TS _ 22) }
  'print' { PT _ (TS _ 23) }
  'return' { PT _ (TS _ 24) }
  'true' { PT _ (TS _ 25) }
  'void' { PT _ (TS _ 26) }
  'while' { PT _ (TS _ 27) }
  '{' { PT _ (TS _ 28) }
  '||' { PT _ (TS _ 29) }
  '}' { PT _ (TS _ 30) }

L_integ  { PT _ (TI $$) }
L_Id { PT _ (T_Id $$) }


%%

Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Id    :: { Id} : L_Id { Id ($1)}

Program :: { Program }
Program : ListStmt { AbsMyC.Prog (reverse $1) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
ListExp :: { [Exp] }
ListExp : {- empty -} { [] }
        | Exp { (:[]) $1 }
        | Exp ',' ListExp { (:) $1 $3 }
ListDec :: { [Dec] }
ListDec : {- empty -} { [] }
        | Dec { (:[]) $1 }
        | Dec ',' ListDec { (:) $1 $3 }
Stmt :: { Stmt }
Stmt : Dec ';' { AbsMyC.SVarDef $1 }
     | Dec '(' ListDec ')' Stmt { AbsMyC.SFunDef $1 $3 $5 }
     | 'while' '(' Exp ')' Stmt { AbsMyC.SWhile $3 $5 }
     | 'if' '(' Exp ')' Stmt { AbsMyC.SIf $3 $5 }
     | 'print' '(' Exp ')' ';' { AbsMyC.SPrint $3 }
     | '{' ListStmt '}' { AbsMyC.SBlock (reverse $2) }
     | Exp ';' { AbsMyC.SExpStmt $1 }
     | Id '=' Exp ';' { AbsMyC.SAss $1 $3 }
     | 'return' Exp ';' { AbsMyC.SReturn $2 }
Exp8 :: { Exp }
Exp8 : Integer { AbsMyC.EILit $1 }
     | 'true' { AbsMyC.ETrue }
     | 'false' { AbsMyC.EFalse }
     | Id { AbsMyC.EVar $1 }
     | Id '(' ListExp ')' { AbsMyC.EApp $1 $3 }
     | '(' Exp ')' { $2 }
Exp7 :: { Exp }
Exp7 : Exp8 '++' { AbsMyC.EIncr $1 }
     | Exp8 '--' { AbsMyC.EDecr $1 }
     | Exp8 { $1 }
Exp6 :: { Exp }
Exp6 : Exp6 '*' Exp7 { AbsMyC.EMul $1 $3 }
     | Exp6 '/' Exp7 { AbsMyC.EDiv $1 $3 }
     | Exp7 { $1 }
Exp5 :: { Exp }
Exp5 : Exp5 '+' Exp6 { AbsMyC.EPlus $1 $3 }
     | Exp5 '-' Exp6 { AbsMyC.EMinus $1 $3 }
     | Exp6 { $1 }
Exp4 :: { Exp }
Exp4 : Exp4 '<' Exp5 { AbsMyC.ELT $1 $3 }
     | Exp4 '<=' Exp5 { AbsMyC.ELEq $1 $3 }
     | Exp4 '>' Exp5 { AbsMyC.EGT $1 $3 }
     | Exp4 '>=' Exp5 { AbsMyC.EGEq $1 $3 }
     | Exp5 { $1 }
Exp3 :: { Exp }
Exp3 : Exp3 '==' Exp4 { AbsMyC.EEq $1 $3 }
     | Exp3 '!=' Exp4 { AbsMyC.ENEq $1 $3 }
     | Exp4 { $1 }
Exp2 :: { Exp }
Exp2 : Exp2 '&&' Exp3 { AbsMyC.EAnd $1 $3 } | Exp3 { $1 }
Exp1 :: { Exp }
Exp1 : Exp1 '||' Exp2 { AbsMyC.EOr $1 $3 } | Exp2 { $1 }
Exp :: { Exp }
Exp : Exp1 { $1 }
Type :: { Type }
Type : 'bool' { AbsMyC.Tbool }
     | 'int' { AbsMyC.Tint }
     | 'void' { AbsMyC.Tvoid }
Dec :: { Dec }
Dec : Type Id { AbsMyC.Decl $1 $2 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

