module Main where

import Data.Map

import SystemF.Inference
import SystemF.Parser

import System.FilePath
import System.Environment

doParse :: String -> Exp
doParse input = case (parseProgram input) of
                     Left err -> error $ "Parse error: " ++ show err
                     Right x -> x

main::IO ()
main = do   (file:_) <- getArgs
            inp <- readFile file
            let prog = doParse inp
            print prog

