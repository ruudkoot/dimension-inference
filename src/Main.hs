module Main where

import Data.Map

import SystemF.Inference
import SystemF.Parser
import SystemF.Types

import System.FilePath
import System.Environment

doParse :: String -> Exp
doParse input = case (parseProgram input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

-- Default type env, nu alleen nog operators
buildEnv :: TyEnv
buildEnv = operatorEnv

main::IO ()
main = do   (file:_) <- getArgs
            inp <- readFile file
            let prog = doParse inp
            putStrLn (pprint prog)
            print (infer buildEnv prog)