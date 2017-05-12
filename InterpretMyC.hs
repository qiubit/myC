module InterpretMyC where

import AbsEvalMyC
import AbsMyC

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


data Value
    = IntVal Integer
    | BoolVal Bool
    deriving (Show)

type Loc = Integer
type Store = Map Loc Value
type Env = Map Id Loc


type InterpreterM a = ReaderT Env (ErrorT String (WriterT [String] (StateT Store IO))) a
runEval :: Env -> Store -> InterpreterM a -> IO ((Either String a, [String]), Store)
runEval env store ev = runStateT (runWriterT (runErrorT (runReaderT ev env))) store


evalIExp :: IExp -> InterpreterM Value

evalIExp (IEILit x) = return $ IntVal x

evalIExp (IEVar n) = do
    env <- ask
    store <- get
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ (show n))
        Just loc -> case Map.lookup loc store of
            Nothing -> throwError ("internal interpreter error: 1")
            Just val -> case val of
                IntVal x -> return $ IntVal x
                _ -> throwError ("type error: expected variable " ++ (show n) ++ " to have type int")

evalIExp (IEPlus e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ IntVal (x+y)

evalIExp (IEMinus e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ IntVal (x-y)

evalIExp (IEMul e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ IntVal (x*y)

evalIExp (IEDiv e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    case y of
        0 -> throwError ("division by zero exception")
        _ -> return $ IntVal (x `div` y)

{--
evalIExp (IEIncr n) = do
    env <- ask
    store <- get
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ (show n))
        Just loc -> case Map.lookup loc store of
            Nothing -> throwError ("internal interpreter error: 2")
            Just val -> case val of
                IntVal x -> do
                    put (Map.insert loc (IntVal (x+1)) store)
                    return $ IntVal x
                _ -> throwError ("type error: expected variable " ++ (show n) ++ " to have type int")

evalIExp (IEDecr n) = do
    env <- ask
    store <- get
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ (show n))
        Just loc -> case Map.lookup loc store of
            Nothing -> throwError ("internal interpreter error: 3")
            Just val -> case val of
                IntVal x -> do
                    put (Map.insert loc (IntVal (x-1)) store)
                    return $ IntVal x
                _ -> throwError ("type error: expected variable " ++ (show n) ++ " to have type int")
--}

evalBExp :: BExp -> InterpreterM Value

evalBExp (BETrue) = do
    return $ BoolVal True

evalBExp (BEFalse) = do
    return $ BoolVal False

evalBExp (BEVar n) = do
    env <- ask
    store <- get
    case Map.lookup n env of
        Nothing -> throwError ("unbound variable: " ++ (show n))
        Just loc -> case Map.lookup loc store of
            Nothing -> throwError ("internal interpreter error: 4")
            Just val -> case val of
                BoolVal x -> return $ BoolVal x
                _ -> throwError ("type error: expected variable " ++ (show n) ++ " to have type bool")

evalBExp (BEOr e1 e2) = do
    BoolVal x <- evalBExp e1
    BoolVal y <- evalBExp e2
    return $ BoolVal (x || y)

evalBExp (BEAnd e1 e2) = do
    BoolVal x <- evalBExp e1
    BoolVal y <- evalBExp e2
    return $ BoolVal (x && y)

evalBExp (BELT e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ BoolVal (x < y)

evalBExp (BELEq e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ BoolVal (x <= y)

evalBExp (BEGT e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ BoolVal (x > y)

evalBExp (BEGEq e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ BoolVal (x >= y)

evalBExp (BEEq e1 e2) = do
    IntVal x <- evalIExp e1
    IntVal y <- evalIExp e2
    return $ BoolVal (x == y)


evalStmt :: EvalStmt -> InterpreterM Env

evalStmt (EvSVarDef (Decl t i)) = do
    env <- ask
    case Map.lookup (Id "?nextLoc") env of
        Nothing -> throwError ("internal intrpreter error: 5")
        Just nextLoc -> do
            store <- get
            case t of
                Tbool -> put $ Map.insert nextLoc (BoolVal False) store
                Tint -> put $ Map.insert nextLoc (IntVal 0) store
            let updatedLoc = (Map.insert (Id "?nextLoc") (nextLoc+1) env) in
                return $ Map.insert i nextLoc updatedLoc


evalStmt (EvSAss i e) = do
    store <- get
    env <- ask
    case Map.lookup i env of
        Nothing -> throwError ("unbound variable assignment: " ++ (show i))
        Just loc -> case Map.lookup loc store of
            Nothing -> throwError ("internal interpreter error: 7")
            Just val -> case e of
                Left iExp -> do
                    iVal <- evalIExp iExp
                    put $ Map.insert loc iVal store
                    return env
                Right bExp -> do
                    bVal <- evalBExp bExp
                    put $ Map.insert loc bVal store
                    return env

evalStmt EvSkipStmt = do
    env <- ask
    return env

evalStmt (EvSWhile e s) = do
    e1 <- evalBExp e
    case e1 of
        BoolVal True -> evalStmts [s, EvSWhile e s]
        BoolVal False -> evalStmt EvSkipStmt

evalStmt (EvSIf e s1) = do
    e1 <- evalBExp e
    case e1 of
        BoolVal True -> evalStmt s1
        BoolVal False -> evalStmt EvSkipStmt

evalStmt (EvSPrint e) = do
    case e of
        Left iExp -> do
            IntVal iVal <- evalIExp iExp
            liftIO $ print iVal
        Right bExp -> do
            BoolVal bVal <- evalBExp bExp
            liftIO $ print bVal
    evalStmt EvSkipStmt

evalStmt (EvSExpStmt e) = do
    case e of
        Left iExp -> do
            evalIExp iExp
        Right bExp -> do
            evalBExp bExp
    evalStmt EvSkipStmt

evalStmt (EvSBlock stmts) = do
    evalStmts stmts
    evalStmt EvSkipStmt


evalStmts :: [EvalStmt] -> InterpreterM Env
evalStmts [] = do
    env <- ask
    return env

evalStmts [s] = do
    env <- evalStmt s
    return env

evalStmts (h:t) = do
    env <- evalStmt h
    env' <- local (const env) (evalStmts t)
    return env'

runProgram :: EvalProgram -> IO ((Either String Env, [String]), Store)
runProgram (EvProg stmts) =
    runEval (Map.insert (Id "?nextLoc") 0 Map.empty) Map.empty (evalStmts stmts)
