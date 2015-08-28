
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Build

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Build where

import Control.Monad.Except

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Simplify
import Language.Qux.Annotated.Syntax
import Language.Qux.TypeChecker

import System.Exit
import System.IO


data Options = Options {
    optTypeCheck    :: Bool,
    argFilePath     :: String
    }

handle :: Options -> IO ()
handle options = do
    let filePath = argFilePath options

    contents <- readFile $ argFilePath options

    case runExcept $ tryParse filePath contents >>= build options of
        Left error  -> hPutStrLn stderr error >> exitFailure
        Right _     -> return ()

tryParse :: FilePath -> String -> Except String (Program SourcePos)
tryParse filePath contents = withExcept show (parse program filePath contents)

build :: Options -> Program a -> Except String ()
build options program = when (optTypeCheck options) (typeCheck program)

typeCheck :: (Program a) -> Except String ()
typeCheck program = withExcept show (check $ sProgram program)

