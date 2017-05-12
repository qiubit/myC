

module AbsEvalMyC where

import AbsMyC

import Data.Either

-- AST after type checker transform

data IExp
    = IEILit Integer
    | IEVar Id
    | IEApp Id [EvalExp]
    | IEIncr IExp
    | IEDecr IExp
    | IEMul IExp IExp
    | IEDiv IExp IExp
    | IEPlus IExp IExp
    | IEMinus IExp IExp
  deriving (Eq, Ord, Show, Read)

data BExp
    = BETrue
    | BEFalse
    | BEVar Id
    | BEApp Id [EvalExp]
    | BELT IExp IExp
    | BELEq IExp IExp
    | BEGT IExp IExp
    | BEGEq IExp IExp
    | BEEq IExp IExp
    | BENEq IExp IExp
    | BEAnd BExp BExp
    | BEOr BExp BExp
  deriving (Eq, Ord, Show, Read)

type EvalExp = Either IExp BExp

data EvalStmt
    = EvSVarDef Dec
    | EvSFunDef Dec [Dec] EvalStmt
    | EvSWhile BExp EvalStmt
    | EvSIf BExp EvalStmt
    | EvSPrint EvalExp
    | EvSBlock [EvalStmt]
    | EvSExpStmt EvalExp
    | EvSAss Id EvalExp
    | EvSReturn EvalExp
    | EvSkipStmt
  deriving (Eq, Ord, Show, Read)

data EvalProgram = EvProg [EvalStmt]
  deriving (Eq, Ord, Show, Read)
