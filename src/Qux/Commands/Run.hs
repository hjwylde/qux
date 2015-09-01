
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Run

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Run where

import Control.Monad.Except

import Language.Qux.Annotated.Simplify
import Language.Qux.Annotated.Syntax
import Language.Qux.Interpreter
import Language.Qux.PrettyPrinter

import Qux.Commands.Build (tryParse)

import System.Exit
import System.IO


data Options = Options {
    optEntry    :: String,
    argFilePath :: FilePath,
    argsExtra   :: [String]
    }

handle :: Options -> IO ()
handle options = do
    let filePath = argFilePath options

    contents <- readFile $ argFilePath options

    case runExcept $ tryParse filePath contents >>= run options of
        Left error      -> hPutStrLn stderr error >> exitFailure
        Right result    -> putStrLn result

run :: Options -> Program a -> Except String String
run options program = return $ render (pPrint result)
    where
        result = exec (sProgram program) (optEntry options) args
        args = map (IntValue . read) (argsExtra options)

