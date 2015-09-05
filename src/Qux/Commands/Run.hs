
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Run

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Run where

import Control.Monad.Except

import Data.List (intercalate)

import Language.Qux.Annotated.Parser
import Language.Qux.Annotated.Simplify
import Language.Qux.Annotated.Syntax
import Language.Qux.Annotated.TypeChecker
import Language.Qux.Interpreter
import Language.Qux.PrettyPrinter

import Qux.Commands.Build (tryParse)

import System.Exit
import System.IO


data Options = Options {
    optEntry        :: String,
    argFilePath     :: FilePath,
    argProgramArgs  :: [String]
    }

handle :: Options -> IO ()
handle options = do
    let filePath = argFilePath options

    contents <- readFile $ argFilePath options

    case runExcept $ tryParse filePath contents >>= run options of
        Left error      -> hPutStrLn stderr error >> exitFailure
        Right result    -> putStrLn result

run :: Options -> Program a -> Except String String
run options program = do
    args <- parseArgs $ argProgramArgs options

    typeCheckArgs args

    let result = exec (sProgram program) (optEntry options) args

    return $ render (pPrint result)

-- TODO (hjw): improve the error message (the source position is wrong)
parseArgs :: [String] -> Except String [Value]
parseArgs = mapM (withExcept show . parse value "command line")

typeCheckArgs :: [Value] -> Except String ()
typeCheckArgs args = when (not $ null errors) $ throwError (intercalate "\n\n" $ map show errors)
    where
        errors = concatMap (\value -> execCheck (checkValue value) emptyContext) args

