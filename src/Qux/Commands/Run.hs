
{-# OPTIONS_HADDOCK hide, prune #-}

{-|
Module      : Qux.Commands.Run

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

module Qux.Commands.Run where

import Control.Monad.Except
import Control.Monad.Identity

import Data.List (intercalate)

import              Language.Qux.Annotated.Parser       hiding (parse)
import qualified    Language.Qux.Annotated.Parser       as P
import              Language.Qux.Annotated.Simplify
import              Language.Qux.Annotated.Syntax
import              Language.Qux.Annotated.TypeChecker
import              Language.Qux.Interpreter            hiding (emptyContext)
import              Language.Qux.PrettyPrinter

import              Qux.Commands.Build (parse)
import qualified    Qux.Commands.Check as Check

import System.Exit
import System.IO
import System.IO.Error


data Options = Options {
    optEntry        :: String,
    optSkipChecks   :: Bool,
    argFilePath     :: FilePath,
    argProgramArgs  :: [String]
    }

handle :: Options -> IO ()
handle options = do
    let filePath = argFilePath options
    contents <- readFile $ argFilePath options

    ethr <- catchIOError
        (runExceptT $ parse filePath contents >>= run options)
        (return . Left . ioeGetErrorString)

    case ethr of
        Left error      -> hPutStrLn stderr error >> exitFailure
        Right result    -> putStrLn result

run :: Options -> Program SourcePos -> ExceptT String IO String
run options program = do
    args <- parseArgs $ argProgramArgs options
    typeCheckArgs args

    when (not $ optSkipChecks options) $ Check.check (checkOptions options) program

    let result = exec (sProgram program) (optEntry options) args

    return $ render (pPrint result)

parseArgs :: [String] -> ExceptT String IO [Value]
parseArgs = mapM (\arg -> mapExceptT (return . runIdentity) (withExcept show (P.parse value "command line" arg)))

typeCheckArgs :: [Value] -> ExceptT String IO ()
typeCheckArgs args = when (not $ null errors) $ throwError (intercalate "\n\n" $ map show errors)
    where
        errors = concatMap (\value -> execCheck (checkValue value) emptyContext) args

checkOptions :: Options -> Check.Options
checkOptions options = Check.Options {
    Check.argFilePaths = [argFilePath options]
    }

