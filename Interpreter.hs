module Main where

import TypeCheckMyC
import ParMyC
import AbsMyC
import AbsEvalMyC
import InterpretMyC

import ErrM

import System.Environment ( getArgs )
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

main = do
    args <- getArgs
    s <- readFile (head args)
    doInterpret s
    putStrLn ""

doInterpret s = do
    case pProgram (myLexer s) of
        Ok tree -> do
            case (runTypeCheck Map.empty (transProgram tree)) of
                (Left x, _) -> putStrLn x
                (Right x, _) -> do
                    _ <- runProgram x
                    return ()
        _ -> putStrLn "PARSE ERROR"

